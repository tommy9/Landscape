#!/usr/bin/env python3
"""Build isolated BBC Micro triangle benchmarks and run them in Beebium."""
import argparse
import json
import subprocess
import time
from pathlib import Path

REVISION = 'd7f03fe69a3f892440e985f063444492a4702a29'
CASES = {
    'coast_4x6': [(40, 80), (44, 82), (41, 86)],
    'grid40_flat_8x2': [(40, 80), (44, 82), (36, 82)],
    'grid40_slope_8x12': [(40, 80), (44, 92), (36, 86)],
    'grid40_steep_8x32': [(40, 80), (44, 112), (36, 90)],
    'grid20_flat_16x4': [(40, 80), (48, 84), (32, 84)],
    'grid10_flat_32x8': [(48, 80), (64, 88), (32, 88)],
}
METHODS = ['mos_mode1', 'mos_mode5', 'xor_original_window', 'xor_bounded', 'basic_mode1']


def address(x, y, base):
    return base + (y // 8) * 320 + (x // 4) * 8 + y % 8


def build(upstream, out, vertices, repeats, assembler):
    out.mkdir(parents=True, exist_ok=True)
    renderer = (upstream / 'source/renderer.asm').read_text()
    # Exact original solid wipe/fill, excluding the unrelated copy and 3D routines.
    original = renderer.split('.wipe \n', 1)[1].split('; copy the back buffer', 1)[0]
    (out / 'original-fill.asm').write_text('.wipe\n' + original)
    (out / 'linedraw5f.asm').write_bytes((upstream / 'source/linedraw5f.asm').read_bytes())
    a = ['x0=&70:y0=&71:x1=&72:y1=&73:scr=&74:err=&76:errs=&77',
         'cnt=&78:ls=&79:dx=&FF:dy=&7A:scrstrt=&7B:c=&7C',
         'ORG &2000', '.start', 'JMP mos1', 'JMP mos5', 'JMP original', 'JMP bounded']
    for name, routine in [('mos1', 'plot1'), ('mos5', 'plot5'), ('original', 'original_one'), ('bounded', 'bounded_one')]:
        a += [f'.{name}', f'LDA #LO({repeats}):STA remaining', f'LDA #HI({repeats}):STA remaining+1',
              f'.{name}_loop', f'JSR {routine}', 'LDA remaining:BNE '+name+'_dec', 'DEC remaining+1',
              f'.{name}_dec', 'DEC remaining', 'LDA remaining:ORA remaining+1', f'BNE {name}_loop', 'RTS']
    a += ['.remaining EQUW 0']
    for mode, scale in [(1, 4), (5, 8)]:
        a += [f'.plot{mode}']
        for i, (x, y) in enumerate(vertices):
            for b in [25, 4 if i < 2 else 85, (x*scale)&255, (x*scale)>>8, ((255-y)*4)&255, ((255-y)*4)>>8]:
                a += [f'LDA #{b}:JSR &FFEE']
        a += ['RTS']
    a += ['.original_one', 'JSR wipe', 'JSR edges', 'JMP fill',
          '.bounded_one', 'JSR clear_box', 'JSR edges', 'JMP fill_box',
          '.edges', 'LDA #&35:STA scrstrt']
    # Original address calculation rotates its initial &80 twice, adding
    # &20 bytes (16 physical pixels): subtract 32 input units so the logical vertex maps to its named pixel.
    for i in range(3):
        for label, val in zip(['x0','y0','x1','y1'], [2*vertices[i][0]-32, vertices[i][1], 2*vertices[(i+1)%3][0]-32, vertices[(i+1)%3][1]]):
            a += [f'LDA #{val}:STA {label}']
        a += ['LDX #1:JSR linedraw5f']
    a += ['RTS', 'INCLUDE "linedraw5f.asm"', 'INCLUDE "original-fill.asm"']
    # Specialised rectangle: byte-aligned horizontal span, with guard scanlines.
    xmin = min(x for x,y in vertices)//4*4
    xmax = max(x for x,y in vertices)//4*4
    ymin = min(y for x,y in vertices)-1
    ymax = max(y for x,y in vertices)+1
    a += ['.clear_box', f'LDX #{(xmax-xmin)*2}', '.clear_column', 'LDA #0']
    for y in range(ymin, ymax+1):
        a += [f'STA &{address(xmin,y,0x3500):04X},X']
    a += ['TXA:SEC:SBC #8:BCC clear_done:TAX:JMP clear_column', '.clear_done', 'RTS',
          '.fill_box', f'LDX #{(xmax-xmin)*2}', '.fill_column', 'LDA #0']
    # One accumulator per four-pixel column, as in the upstream filler.
    # Specialised scanlines: no run-time bounding-box computation.
    for y in range(ymin, ymax+1):
        a += [f'EOR &{address(xmin,y,0x3500):04X},X:STA &{address(xmin,y,0x5800):04X},X']
    a += ['TXA:SEC:SBC #8:BCC fill_done:TAX:JMP fill_column', '.fill_done']
    a += ['RTS', '.end', 'ASSERT end <= &3000', 'SAVE "BENCH", start, end',
          'PUTBASIC "bench.bas", "RUN"', 'PUTFILE "boot.txt", "!Boot", &FFFFFF', 'PRINT "Code end ", ~end']
    (out / 'bench.asm').write_text('\n'.join(a)+'\n')
    basic = ['HIMEM=&2000', '*LOAD BENCH', '?&8FF=0']
    for method in range(5):
        basic += [f'MODE {1 if method in (0,4) else 5}', 'VDU 23;8202;0;0;0;', 'GCOL 0,1', 'T%=TIME']
        if method < 4:
            basic += [f'CALL &{0x2000+method*3:X}']
        else:
            basic += [f'FOR I%=1 TO {repeats}']
            basic += [f'PLOT {4 if i<2 else 85},{x*4},{(255-y)*4}' for i,(x,y) in enumerate(vertices)]
            basic += ['NEXT']
        basic += [f'!&900=TIME-T%', f'?&8FF={method+1}', 'REPEAT UNTIL ?&8FF=0']
    basic += ['PRINT "Benchmark complete"', 'END']
    (out / 'bench.bas').write_text('\n'.join(basic)+'\n')
    (out / 'boot.txt').write_bytes(b'CHAIN "RUN"\r')
    subprocess.run([assembler, '-i', 'bench.asm', '-do', 'bench.ssd', '-opt', '3'], cwd=out, check=True)
    return (xmin, ymin, xmax, ymax)


def run(disk, output, repeats, server=None, rom_dir=None, methods=None, unit="triangle"):
    methods = METHODS if methods is None else methods
    from beebium.client import Beebium
    import beebium.server
    dfs = (rom_dir or Path(beebium.server.__file__).resolve().parent/'_bundle/share/beebium/roms')/'acorn-dfs_2_26.rom'
    kwargs = dict(extra_args=['--fdc','acorn-1770','--sideways',f'14:rom:{dfs}','--auto-boot','--floppy',f'0:{disk}'])
    if server:
        kwargs['server'] = str(server)
    results = {}
    images = {}
    with Beebium.launch(**kwargs) as bbc:
        bbc.debugger.ensure_running()
        bbc.system.set_speed_multiplier(0.0)
        for stage, name in enumerate(methods, 1):
            deadline = time.monotonic()+60
            while bbc.memory.address.peek[0x8ff] != stage:
                if time.monotonic()>deadline:
                    raise RuntimeError(f'Timeout at {name}: {bbc.video.screen_text().text}')
                time.sleep(0.02)
            bbc.debugger.stop()
            ticks = int.from_bytes(bbc.memory.address.peek.read(0x900,4), 'little')
            base = 0x3000 if name.endswith('mode1') else 0x5800
            ram = bbc.memory.address.peek.read(base, 0x8000-base)
            (output/f'{name}.bin').write_bytes(ram)
            # Decode native pixels directly, avoiding frame timing at the handshake.
            from PIL import Image
            width = 320 if base==0x3000 else 160
            im = Image.new('RGB',(width,256))
            count = 0
            for y in range(256):
                for x in range(width):
                    offset = (y//8)*(width*2)+(x//4)*8+y%8
                    byte = ram[offset]
                    shift = 3-x%4
                    color = ((byte>>shift)&1) | (((byte>>(shift+4))&1)<<1)
                    count += color != 0
                    im.putpixel((x,y), [(0,0,0),(255,60,60),(255,255,0),(255,255,255)][color])
            im.save(output/f'{name}.png')
            images[name] = im
            results[name] = dict(ticks=ticks, **{f"microseconds_per_{unit}": ticks*10000/repeats}, nonzero_pixels=count)
            bbc.memory.address.bus[0x8ff]=0
            bbc.debugger.run()
    # MOS modes must cover exactly the same physical pixels. The bounded
    # variant must reproduce the upstream full-window result, not just run fast.
    assert images['mos_mode1'].crop((0,0,160,256)).tobytes() == images['mos_mode5'].tobytes()
    if 'basic_mode1' in images:
        assert images['basic_mode1'].tobytes() == images['mos_mode1'].tobytes()
    if 'xor_unmerged' in images:
        assert images['xor_unmerged'].tobytes() == images['xor_bounded'].tobytes()
    assert images['xor_original_window'].tobytes() == images['xor_bounded'].tobytes()
    assert all(result['nonzero_pixels'] > 0 for result in results.values())
    return results


def comparison_image(output, cases):
    from PIL import Image, ImageDraw
    image = Image.new('RGB', (660, len(cases)*205+50), '#161b22')
    draw = ImageDraw.Draw(image)
    draw.text((15,12), 'Equal native pixel scale: MOS PLOT 85 (left) / bounded XOR (right)', fill='white')
    for i, (name, case) in enumerate(cases.items()):
        y = 50+i*205
        draw.text((15,y), name, fill='white')
        for col, method in enumerate(['mos_mode1', 'xor_bounded']):
            source = Image.open(output/name/f'{method}.png')
            crop = source.crop((28,76,68,116)).resize((160,160), Image.Resampling.NEAREST)
            image.paste(crop, (40+col*320,y+22))
            count = case['methods'][method]['nonzero_pixels']
            draw.text((205+col*320,y+35), f'{count} pixels', fill='white')
    image.save(output/'comparison.png')


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--upstream',type=Path,required=True)
    p.add_argument('--output',type=Path,default=Path('.beebium-test/triangle-bench'))
    p.add_argument('--repeats',type=int,default=1000)
    p.add_argument('--assembler',default='BeebAsm')
    p.add_argument('--server',type=Path)
    p.add_argument('--rom-dir',type=Path)
    p.add_argument('--case',choices=CASES)
    args=p.parse_args()
    if not 1<=args.repeats<=65535: p.error('--repeats must be 1..65535')
    actual=subprocess.check_output(['git','-C',str(args.upstream),'rev-parse','HEAD'],text=True).strip()
    if actual!=REVISION: p.error(f'Expected upstream revision {REVISION}, found {actual}')
    all_results={'upstream_revision':actual,'repeats':args.repeats,'cases':{}}
    for name, vertices in CASES.items():
        if args.case and name!=args.case: continue
        out=args.output.resolve()/name
        box=build(args.upstream.resolve(),out,vertices,args.repeats,args.assembler)
        results=run(out/'bench.ssd',out,args.repeats,args.server,args.rom_dir)
        all_results['cases'][name]={'vertices':vertices,'box':box,'methods':results}
        print(name, json.dumps(results),flush=True)
        (args.output/'results.json').write_text(json.dumps(all_results,indent=2)+'\n')
    comparison_image(args.output, all_results['cases'])

if __name__=='__main__':
    main()
