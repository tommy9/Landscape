#!/usr/bin/env python3
"""Compare 18 MOS triangles against one XOR sweep of a 3x3 checkerboard."""
import argparse
import json
import subprocess
from pathlib import Path
from collections import Counter

from benchmark import REVISION, address, run

METHODS = ['mos_mode1', 'mos_mode5', 'xor_original_window', 'xor_bounded', 'xor_unmerged']


def geometry(step, x_offset=0):
    def point(i, j):
        return (80+x_offset+(i-j)*step, 80+(i+j)*(step//2))
    cells = []
    edges = {}
    unmerged = []
    for i in range(3):
        for j in range(3):
            corners = [point(i,j), point(i+1,j), point(i+1,j+1), point(i,j+1)]
            colour = 1 if (i+j)%2 == 0 else 2
            cells.append(dict(corners=corners, colour=colour))
            for a,b in zip(corners, corners[1:]+corners[:1]):
                # Canonical ordering also guarantees the identical edge raster for
                # both cells. Internal transition = colour on side A XOR side B.
                edge = tuple(sorted((a,b)))
                edges[edge] = edges.get(edge, 0) ^ colour
                unmerged.append((*edge, colour))
    merged = [(a,b,c) for (a,b),c in edges.items() if c]
    assert len(cells)==9 and len(merged)==24 and len(unmerged)==36
    return cells, merged, unmerged


def build(upstream, out, step, repeats, assembler, x_offset=0):
    out.mkdir(parents=True, exist_ok=True)
    cells, merged, unmerged = geometry(step, x_offset)
    renderer = (upstream/'source/renderer.asm').read_text()
    original = renderer.split('.wipe \n',1)[1].split('; copy the back buffer',1)[0]
    (out/'original-fill.asm').write_text('.wipe\n'+original)
    (out/'linedraw5f.asm').write_bytes((upstream/'source/linedraw5f.asm').read_bytes())
    a = ['x0=&70:y0=&71:x1=&72:y1=&73:scr=&74:err=&76:errs=&77',
         'cnt=&78:ls=&79:dx=&FF:dy=&7A:scrstrt=&7B:c=&7C',
         'stream=&80:edgeptr=&82', 'ORG &1C00', '.start']
    a += [f'JMP method{i}' for i in range(5)]
    for i, routine in enumerate(['plot1','plot5','original_one','bounded_one','unmerged_one']):
        a += [f'.method{i}',f'LDA #LO({repeats}):STA remaining',f'LDA #HI({repeats}):STA remaining+1',
              f'.repeat{i}', f'JSR {routine}', f'LDA remaining:BNE dec{i}', 'DEC remaining+1',
              f'.dec{i}', 'DEC remaining', 'LDA remaining:ORA remaining+1',f'BNE repeat{i}','RTS']
    a += ['.remaining EQUW 0', '.edgeindex EQUB 0']
    for mode, scale in [(1,4),(5,8)]:
        a += [f'.plot{mode}',f'LDA #LO(commands{mode}):STA stream',f'LDA #HI(commands{mode}):STA stream+1','JMP send_commands']
        commands = []
        for cell in cells:
            commands += [18,0,cell['colour']]
            v=cell['corners']
            # Two MOVEs then two PLOT85s: reuse the middle horizontal diagonal.
            for n,(x,y) in enumerate([v[0],v[1],v[3],v[2]]):
                xx,yy=x*scale,(255-y)*4
                commands += [25,4 if n<2 else 85,xx&255,xx>>8,yy&255,yy>>8]
        assert len(commands)==243
        a += [f'.commands{mode}', 'EQUB '+','.join(map(str,commands))]
    a += ['.send_commands','LDY #0','.send_byte','LDA (stream),Y:JSR &FFEE', 'INY:CPY #243:BNE send_byte','RTS']
    a += ['.original_one','JSR wipe:JSR edges:JMP fill',
          '.bounded_one','JSR clear_box:JSR edges:JMP fill_box',
          '.unmerged_one','JSR clear_box:JSR edges_unmerged:JMP fill_box']
    for label, data in [('edges',merged),('edges_unmerged',unmerged)]:
        a += [f'.{label}',f'LDA #LO({label}_data):STA edgeptr',f'LDA #HI({label}_data):STA edgeptr+1',
              f'LDA #{len(data)*5}:STA edge_limit+1','JMP draw_edges',f'.{label}_data']
        for p,q,c in data:
            a += ['EQUB '+','.join(map(str,[p[0]*2-32,p[1],q[0]*2-32,q[1],c]))]
    a += ['.draw_edges','LDA #&35:STA scrstrt','LDA #0:STA edgeindex', '.edge_loop', 'LDY edgeindex',
          'LDA (edgeptr),Y:STA x0:INY','LDA (edgeptr),Y:STA y0:INY',
          'LDA (edgeptr),Y:STA x1:INY','LDA (edgeptr),Y:STA y1:INY',
          'LDA (edgeptr),Y:TAX:INY:STY edgeindex','JSR linedraw5f',
          'LDA edgeindex','.edge_limit','CMP #0:BNE edge_loop','RTS',
          'INCLUDE "linedraw5f.asm"','INCLUDE "original-fill.asm"']
    vertices = [v for cell in cells for v in cell['corners']]
    xmin=min(x for x,y in vertices)//4*4
    xmax=max(x for x,y in vertices)//4*4
    ymin=min(y for x,y in vertices)-1
    ymax=max(y for x,y in vertices)+1
    a += ['.clear_box',f'LDX #{(xmax-xmin)*2}', '.clear_column','LDA #0']
    for y in range(ymin,ymax+1):
        a += [f'STA &{address(xmin,y,0x3500):04X},X']
    a += ['TXA:SEC:SBC #8:BCC clear_done:TAX:JMP clear_column','.clear_done','RTS',
          '.fill_box',f'LDX #{(xmax-xmin)*2}', '.fill_column','LDA #0']
    for y in range(ymin,ymax+1):
        a += [f'EOR &{address(xmin,y,0x3500):04X},X:STA &{address(xmin,y,0x5800):04X},X']
    a += ['TXA:SEC:SBC #8:BCC fill_done:TAX:JMP fill_column','.fill_done','RTS','.end',
          'PRINT "Code end ", ~end','ASSERT end <= &3000','SAVE "BENCH", start, end',
          'PUTBASIC "bench.bas", "RUN"','PUTFILE "boot.txt", "!Boot", &FFFFFF']
    (out/'bench.asm').write_text('\n'.join(a)+'\n')
    basic=['HIMEM=&1C00','*LOAD BENCH','?&8FF=0','FOR M%=0 TO 4',
           'IF M%=0 THEN MODE 1 ELSE MODE 5', 'VDU 23;8202;0;0;0;', 'T%=TIME',
           'CALL &1C00+3*M%', '!&900=TIME-T%', '?&8FF=M%+1',
           'REPEAT UNTIL ?&8FF=0','NEXT','PRINT "Patch complete"','END']
    (out/'bench.bas').write_text('\n'.join(basic)+'\n')
    (out/'boot.txt').write_bytes(b'CHAIN "RUN"\r')
    subprocess.run([assembler,'-i','bench.asm','-do','bench.ssd','-opt','3'],cwd=out,check=True)
    return dict(cells=cells,unique_edges=len(merged),unmerged_edges=len(unmerged),mos_triangles=18,
                native_span=[step*6,step*3],box=[xmin,ymin,xmax,ymax],
                sweep_bytes=((xmax-xmin)//4+1)*(ymax-ymin+1))


def verify(out, cells, step):
    from PIL import Image
    counts={}
    for name in METHODS:
        image=Image.open(out/f'{name}.png')
        colors=Counter(image.get_flattened_data() if hasattr(image, "get_flattened_data") else image.getdata())
        assert set(colors)<= {(0,0,0),(255,60,60),(255,255,0)}, (name,colors)
        for cell in cells:
            cx=sum(x for x,y in cell['corners'])//4
            cy=sum(y for x,y in cell['corners'])//4
            expected=(255,60,60) if cell['colour']==1 else (255,255,0)
            assert image.getpixel((cx,cy))==expected, (name,cx,cy)
        # A flat convex patch must have no black holes between its outer edges.
        for y in range(image.height):
            occupied=[x for x in range(image.width) if image.getpixel((x,y))!=(0,0,0)]
            if occupied:
                assert len(occupied)==max(occupied)-min(occupied)+1, (name,y)
        counts[name]={'red':colors[(255,60,60)],'yellow':colors[(255,255,0)]}
        if name.startswith('xor_'):
            assert counts[name] == {'red':5*step*step, 'yellow':4*step*step}, (name,counts[name])
    return counts


def illustration(output, cases):
    from PIL import Image,ImageDraw
    canvas=Image.new('RGB',(950,270*len(cases)+45),'#161b22')
    draw=ImageDraw.Draw(canvas)
    draw.text((15,12),'3x3 isometric patch - equal native pixel scale; 4x enlargement',fill='white')
    for i,(name,case) in enumerate(cases.items()):
        y=45+i*270
        draw.text((15,y),name,fill='white')
        for col,method in enumerate(['mos_mode1','xor_bounded']):
            image=Image.open(output/name/f'{method}.png')
            canvas.paste(image.crop((28,76,136,132)).resize((432,224),Image.Resampling.NEAREST),(15+470*col,y+36))
            ms=case['methods'][method]['microseconds_per_patch']/1000
            draw.text((15+470*col,y+18),f'{method}: {ms:.2f} ms / whole patch',fill='white')
    canvas.save(output/'patch-comparison.png')


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--upstream',type=Path,required=True)
    p.add_argument('--output',type=Path,default=Path('.beebium-test/patch-bench'))
    p.add_argument('--repeats',type=int,default=200)
    p.add_argument('--assembler',default='BeebAsm')
    p.add_argument('--server',type=Path)
    p.add_argument('--rom-dir',type=Path)
    p.add_argument('--step',type=int,choices=[4,8,16])
    args=p.parse_args()
    if not 1<=args.repeats<=65535: p.error('--repeats must be 1..65535')
    actual=subprocess.check_output(['git','-C',str(args.upstream),'rev-parse','HEAD'],text=True).strip()
    if actual!=REVISION: p.error(f'Expected upstream revision {REVISION}')
    results={'upstream_revision':actual,'repeats':args.repeats,'unit':'whole 9-cell patch','cases':{}}
    for step in [4,8,16]:
        if args.step and step!=args.step: continue
        name=f'grid{320//(step*2)}_3x3'
        out=args.output.resolve()/name
        case=build(args.upstream.resolve(),out,step,args.repeats,args.assembler)
        case['methods']=run(out/'bench.ssd',out,args.repeats,args.server,args.rom_dir,METHODS,unit='patch')
        case['colour_counts']=verify(out,case['cells'],step)
        results['cases'][name]=case
        print(name,json.dumps(case['methods']),flush=True)
        (args.output/'patch-results.json').write_text(json.dumps(results,indent=2)+'\n')
    illustration(args.output,results['cases'])

if __name__=='__main__':
    main()
