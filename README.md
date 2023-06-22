# Landscape
BBC Micro based landscape rendering

## Background
The original version of this program was published in Acorn User edition 86 from September 1989. I fondly remember typing it in laboriously and generating various sample landscapes with it at the time. The program was written by David Lawrence and is copyright Acorn User magazine. 

<img src="https://camo.githubusercontent.com/3aef2fe3f5b89f005792ba7b2d8a2ca990f8554ef97b7c5518c8b0e3e8c49e86/68747470733a2f2f69613830323530382e75732e617263686976652e6f72672f426f6f6b5265616465722f426f6f6b526561646572496d616765732e7068703f7a69703d2f302f6974656d732f61636f726e2d757365722d6d6167617a696e652f41636f726e557365723038362d53657038395f6a70322e7a69702666696c653d41636f726e557365723038362d53657038395f6a70322f41636f726e557365723038362d53657038395f303030302e6a70322669643d61636f726e2d757365722d6d6167617a696e65267363616c653d3226726f746174653d30" alt="Cover of Acorn User magazine from September 1989" width="200" height="300">

The original program is saved here as BeebVersion.bbc. With a bit of compression, we can even enter it into an online emulator via the url. Try it out here: [Landscape](https://bbcmic.ro/#%7B%22v%22%3A1%2C%22program%22%3A%22%C3%AB1%3A%C3%AF23%3B8202%3B0%3B0%3B0%3B%3A%C3%B2i%3A%C3%B2f%28f%29%3A%C3%B2l%3A%C3%B2d%3A%C3%A0%5Cn%C3%9D%C3%B2i%3AA%25%3D10%3AD%25%3D64%3AE%25%3D32%3Az%25%3D4%3Ac%3D%C2%B9%3Al%3D%C2%A3%3Af%3D50%3AF%3D3%3A%3A%C3%9El%28A%25%2CA%25%29%3A%C3%9Es%25%282%29%2Cx%25%282%29%2Cy%25%282%29%2Ch%283%29%3A%C3%AF19%2C3%2C4%3B0%3B%3Ab%3D0%3AD%3D3%3A%C3%A1%5Cn%C3%9D%C3%B2f%28n%25%29%3A%C3%A3i%25%3D1%C2%B8n%25%3A%C3%B1%C6%8A0%2C0%29%3B%5C%22Faults%20left%3A%5C%22%3Bn%25-i%25%3B%5C%22%20%5C%22%3A%C3%A7c%20%C3%B2c%5Cn%C3%A7l%20%C3%B2n%5Cn%C3%AD%3A%C3%A1%5Cn%C3%9D%C3%B2c%3Ax%25%3D%C2%B3%28A%25%29-1%3Ay%25%3D%C2%B3%28A%25%29-1%3Ar%25%3D%28%C2%B3%28A%25%29-1%29%5E2%3Aa%3D%C2%B4%C2%B3*%C2%B3%281%29*F%3A%C3%A3X%25%3D0%C2%B8A%25%3AG%3D%28X%25-x%25%29%5E2%3A%C3%A7G%3Cr%25%C3%B2o%5Cn%C3%AD%3A%C3%A1%5Cn%C3%9D%C3%B2o%3AH%3D%C2%B6%28r%25-G%29%3Ay%3Dy%25-H%3AJ%3Dy%25%2BH%3A%C3%A7y%3C0y%3D0%5Cn%C3%A7J%3E%3DA%25J%3DA%25-1%5Cn%C3%A3Y%25%3Dy%20%C2%B8J%3Al%28X%25%2CY%25%29%3Dl%28X%25%2CY%25%29%2Ba%3A%C3%AD%3A%C3%A1%5Cn%C3%9D%C3%B2n%3A%C3%B5%3As%25%281%29%3D%C2%B3%284%29%3As%25%282%29%3D%C2%B3%284%29%3A%C3%BDs%25%281%29%3C%3Es%25%282%29%3A%C3%B5%3A%C3%A3J%25%3D1%C2%B82%3A%C3%A7s%25%28J%25%29%3D1x%25%28J%25%29%3D0%3Ay%25%28J%25%29%3D%C2%B3%28A%25%29-1%5Cn%C3%A7s%25%28J%25%29%3D2x%25%28J%25%29%3D%C2%B3%28A%25%29-1%3Ay%25%28J%25%29%3DA%25%5Cn%C3%A7s%25%28J%25%29%3D3x%25%28J%25%29%3DA%25%3Ay%25%28J%25%29%3D%C2%B3%28A%25%29-1%5Cn%C3%A7s%25%28J%25%29%3D4x%25%28J%25%29%3D%C2%B3%28A%25%29-1%3Ay%25%28J%25%29%3D0%5Cn%C3%AD%3A%C3%BDx%25%281%29%3C%3Ex%25%282%29%C6%80y%25%281%29%3C%3Ey%25%282%29%3AM%3D%28y%25%282%29-y%25%281%29%29%2F%28x%25%282%29-x%25%281%29%29%3AC%3Dy%25%281%29-M*x%25%281%29%3Aa%3D%C2%B4%C2%B3*%C2%B3%281%29*F%3A%C3%A3X%25%3D0%C2%B8A%25%3AY%25%3DM*X%25%2BC%3A%C3%A7Y%25%3C0Y%25%3D0%5Cn%C3%A7Y%25%3C%3DA%25%C3%B2e%5Cn%C3%AD%3A%C3%A1%3A%3A%C3%B2e%3A%C3%A3y%25%3DY%25%C2%B8A%25%3Al%28X%25%2Cy%25%29%3Dl%28X%25%2Cy%25%29%2Ba%3A%C3%AD%3A%C3%A1%5Cn%C3%9D%C3%B2l%3AE%3D0%3Ap%3D0%3AA%3D0%3Al%28A%25%2CA%25%29%3Dl%28A%25-1%2CA%25-1%29%3A%C3%A3X%25%3D0%C2%B8A%25-1%3Al%28X%25%2CA%25%29%3Dl%28X%25%2CA%25-1%29%3Al%28A%25%2CX%25%29%3Dl%28A%25-1%2CX%25%29%3AA%3DA%2Bl%28X%25%2CA%25%29%2Bl%28A%25%2CX%25%29%3A%C3%A3Y%25%3D0%C2%B8A%25-1%3AA%3DA%2Bl%28X%25%2CY%25%29%3AB%3D%28l%28X%25%2CY%25%29%2Bl%28X%25%2B1%2CY%25%29%2Bl%28X%25%2B1%2CY%25%2B1%29%2Bl%28X%25%2CY%25%2B1%29%29%2F4%3A%C3%A7B%3CE%20E%3DB%5Cn%C3%A7B%3Ep%20p%3DB%5Cn%C3%AD%3A%C3%AD%3Aw%3DA%2F%28A%25*A%25%29%3A%C3%A1%5Cn%C3%9D%C3%B2d%3A%C3%9B%3Ac%25%3D1%3A%C3%A3I%25%3D0%C2%B8A%25-1%3A%C3%A3J%25%3D0%C2%B8A%25-1%3A%C3%B2p%28I%25%2CJ%25%29%3A%C3%A7I%25%3DA%25-1%C3%B2s%28c%25%29%5Cnc%25%3D3-c%25%3A%C3%AD%3Ac%25%3D3-c%25%3A%C3%B2g%28c%25%29%3A%C3%AD%3A%C3%A60%2Cb%3A%C3%B23%284%2CA%25%2CA%25%2Cw%29%3A%C3%B23%285%2CA%25%2CA%25%2CE%29%3A%C3%B2A%285%2CA%25%2CA%25%29%3A%C3%A1%5Cn%C3%9D%C3%B2p%28X%25%2CY%25%29%3Ah%280%29%3Dl%28X%25%2CY%25%29-w%3Ah%281%29%3Dl%28X%25%2B1%2CY%25%29-w%3Ah%282%29%3Dl%28X%25%2B1%2CY%25%2B1%29-w%3Ah%283%29%3Dl%28X%25%2CY%25%2B1%29-w%3AF%24%3D%5C%22FN%5C%22%3A%C3%A3i%25%3D0%C2%B83%3A%C3%A7h%28i%25%29%3C0F%24%3DF%24%2B%5C%22b%5C%22%C6%8BF%24%3DF%24%2B%5C%22a%5C%22%5Cn%C3%AD%3A%C3%B2a%3A%C3%A60%2Cc%25%3An%3D%C2%A0%28F%24%29%3A%C3%A1%5Cn%C3%9D%C3%B2g%28c%25%29%3A%C3%A60%2CD%3A%C3%B23%284%2CI%25%2CA%25%2CE%29%3A%C3%B23%284%2CI%25%2B1%2CA%25%2CE%29%3A%C3%B23%2885%2CI%25%2CA%25%2Cw%29%3A%C3%B23%2885%2CI%25%2B1%2CA%25%2Cw%29%3A%C3%A60%2Cb%3A%C3%B23%285%2CI%25%2CA%25%2Cw%29%3A%C3%A60%2Cc%25%3A%C3%B23%284%2CI%25%2CA%25%2CE%29%3A%C3%B23%284%2CI%25%2B1%2CA%25%2CE%29%3A%C3%B2A%2885%2CI%25%2CA%25%29%3A%C3%B2A%2885%2CI%25%2B1%2CA%25%29%3A%C3%A60%2Cb%3A%C3%B2A%285%2CI%25%2CA%25%29%3A%C3%A1%5Cn%C3%9D%C3%B2s%28c%25%29%3A%C3%A60%2CD%3A%C3%B23%284%2CA%25%2CJ%25%2CE%29%3A%C3%B23%284%2CA%25%2CJ%25%2B1%2CE%29%3A%C3%B23%2885%2CA%25%2CJ%25%2Cw%29%3A%C3%B23%2885%2CA%25%2CJ%25%2B1%2Cw%29%3A%C3%A60%2Cb%3A%C3%B23%285%2CA%25%2CJ%25%2Cw%29%3A%C3%A60%2Cc%25%3A%C3%B23%284%2CA%25%2CJ%25%2CE%29%3A%C3%B23%284%2CA%25%2CJ%25%2B1%2CE%29%3A%C3%B2A%2885%2CA%25%2CJ%25%29%3A%C3%B2A%2885%2CA%25%2CJ%25%2B1%29%3A%C3%A60%2Cb%3A%C3%B2A%285%2CA%25%2CJ%25%29%3A%C3%A1%5Cn%C3%9D%C3%B2a%3A%C3%A60%2CD%3A%C3%B23%284%2CX%25%2CY%25%2Cw%29%3A%C3%B23%284%2CX%25%2B1%2CY%25%2Cw%29%3A%C3%B23%2885%2CX%25%2CY%25%2B1%2Cw%29%3A%C3%B23%2885%2CX%25%2B1%2CY%25%2B1%2Cw%29%3A%C3%A1%3A%5Cn%C3%9D%C2%A4aaaa%3A%C3%B2A%284%2CX%25%2CY%25%29%3A%C3%B2A%284%2CX%25%2B1%2CY%25%29%3A%C3%B2A%2885%2CX%25%2CY%25%2B1%29%3A%C3%B2A%2885%2CX%25%2B1%2CY%25%2B1%29%3A%C3%A60%2Cb%3A%C3%B2A%285%2CX%25%2B1%2CY%25%29%3A%C3%B2A%285%2CX%25%2CY%25%29%3A%C3%B2A%285%2CX%25%2CY%25%2B1%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%2B1%29%3A%3D0%5Cn%C3%9D%C2%A4aaab%3A%C3%B2A%284%2CX%25%2CY%25%29%3A%C3%B2A%284%2CX%25%2B1%2CY%25%29%3A%C3%B2m%2885%2C3%29%3A%C3%B2A%2885%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2m%2885%2C2%29%3A%C3%A60%2Cb%3A%C3%B2m%285%2C3%29%3A%C3%B2A%285%2CX%25%2CY%25%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2m%285%2C2%29%3A%3D0%5Cn%C3%9D%C2%A4aaba%3A%C3%B2A%284%2CX%25%2CY%25%29%3A%C3%B2A%284%2CX%25%2CY%25%2B1%29%3A%C3%B2A%2885%2CX%25%2B1%2CY%25%29%3A%C3%B2m%2885%2C2%29%3A%C3%B2m%2885%2C1%29%3A%C3%A60%2Cb%3A%C3%B2m%285%2C2%29%3A%C3%B2A%285%2CX%25%2CY%25%2B1%29%3A%C3%B2A%285%2CX%25%2CY%25%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%29%3A%C3%B2m%285%2C1%29%3A%3D0%5Cn%C3%9D%C2%A4aabb%3A%C3%B2A%284%2CX%25%2CY%25%29%3A%C3%B2A%284%2CX%25%2B1%2CY%25%29%3A%C3%B2m%2885%2C3%29%3A%C3%B2m%2885%2C1%29%3A%C3%A60%2Cb%3A%C3%B2m%285%2C3%29%3A%C3%B2A%285%2CX%25%2CY%25%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%29%3A%C3%B2m%285%2C1%29%3A%3D0%5Cn%C3%9D%C2%A4abaa%3A%C3%B2A%284%2CX%25%2CY%25%2B1%29%3A%C3%B2A%284%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2A%2885%2CX%25%2CY%25%29%3A%C3%B2m%2885%2C1%29%3A%C3%B2m%2885%2C0%29%3A%C3%A60%2Cb%3A%C3%B2m%285%2C1%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2A%285%2CX%25%2CY%25%2B1%29%3A%C3%B2A%285%2CX%25%2CY%25%29%3A%C3%B2m%285%2C0%29%3A%3D0%5Cn%C3%9D%C2%A4abab%3A%C3%B2t%280%29%3A%C3%A60%2Cc%25%3A%C3%B2t%282%29%3A%3D0%5Cn%C3%9D%C2%A4abba%3A%C3%B2A%284%2CX%25%2CY%25%29%3A%C3%B2A%284%2CX%25%2CY%25%2B1%29%3A%C3%B2m%2885%2C0%29%3A%C3%B2m%2885%2C2%29%3A%C3%A60%2Cb%3A%C3%B2A%285%2CX%25%2CY%25%2B1%29%3A%C3%B2A%285%2CX%25%2CY%25%29%3A%C3%B2m%285%2C0%29%3A%C3%B2m%285%2C2%29%3A%3D0%3A%5Cn%C3%9D%C2%A4abbb%3A%C3%B2t%280%29%3A%3D0%5Cn%C3%9D%C2%A4baaa%3A%C3%B2m%284%2C3%29%3A%C3%B2m%284%2C0%29%3A%C3%B2A%2885%2CX%25%2CY%25%2B1%29%3A%C3%B2A%2885%2CX%25%2B1%2CY%25%29%3A%C3%B2A%2885%2CX%25%2B1%2CY%25%2B1%29%3A%C3%A60%2Cb%3A%C3%B2A%285%2CX%25%2CY%25%2B1%29%3A%C3%B2m%285%2C3%29%3A%C3%B2m%285%2C0%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%2B1%29%3A%3D0%5Cn%C3%9D%C2%A4baab%3A%C3%B2A%284%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2A%284%2CX%25%2B1%2CY%25%29%3A%C3%B2m%2885%2C2%29%3A%C3%B2m%2885%2C0%29%3A%C3%A60%2Cb%3A%C3%B2m%285%2C2%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%29%3A%C3%B2m%285%2C0%29%3A%3D0%5Cn%C3%9D%C2%A4baba%3A%C3%B2t%281%29%3A%C3%A60%2Cc%25%3A%C3%B2t%283%29%3A%3D0%5Cn%C3%9D%C2%A4babb%3A%C3%B2t%281%29%3A%3D0%5Cn%C3%9D%C2%A4bbaa%3A%C3%B2A%284%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2A%284%2CX%25%2CY%25%2B1%29%3A%C3%B2m%2885%2C1%29%3A%C3%B2m%2885%2C3%29%3A%C3%A60%2Cb%3A%C3%B2m%285%2C1%29%3A%C3%B2A%285%2CX%25%2B1%2CY%25%2B1%29%3A%C3%B2A%285%2CX%25%2CY%25%2B1%29%3A%C3%B2m%285%2C3%29%3A%3D0%5Cn%C3%9D%C2%A4bbab%3A%C3%B2t%282%29%3A%3D0%5Cn%C3%9D%C2%A4bbba%3A%C3%B2t%283%29%3A%3D0%5Cn%C3%9D%C2%A4bbbb%3A%3D0%5Cn%C3%9D%C3%B2B%28x%25%2Cy%25%2CB%25%2CC%25%29%3A%C3%B2A%284%2Cx%25%2Cy%25%29%3A%C3%B2m%284%2CB%25%29%3A%C3%B2m%2885%2CC%25%29%3A%C3%A60%2Cb%3A%C3%B2m%285%2CB%25%29%3A%C3%B2A%285%2Cx%25%2Cy%25%29%3A%C3%B2m%285%2CC%25%29%3A%C3%A1%5Cn%C3%9D%C3%B2t%28t%25%29%3A%C3%A7t%25%3D0%C3%B2B%28X%25%2CY%25%2C3%2C0%29%5Cn%C3%A7t%25%3D1%C3%B2B%28X%25%2B1%2CY%25%2C0%2C1%29%5Cn%C3%A7t%25%3D2%C3%B2B%28X%25%2B1%2CY%25%2B1%2C1%2C2%29%5Cn%C3%A7t%25%3D3%C3%B2B%28X%25%2CY%25%2B1%2C2%2C3%29%5Cn%C3%A1%5Cn%C3%9D%C3%B2m%28o%25%2Cs%25%29%3A%C3%A7s%25%3D0%C3%B2C%28o%25%2CX%25%2CY%25%2C1%2C0%2C0%2C1%29%5Cn%C3%A7s%25%3D1%C3%B2C%28o%25%2CX%25%2B1%2CY%25%2C0%2C1%2C1%2C2%29%5Cn%C3%A7s%25%3D2%C3%B2C%28o%25%2CX%25%2CY%25%2B1%2C1%2C0%2C3%2C2%29%5Cn%C3%A7s%25%3D3%C3%B2C%28o%25%2CX%25%2CY%25%2C0%2C1%2C0%2C3%29%5Cn%C3%A1%5Cn%C3%9D%C3%B2C%28o%25%2Cx%2CK%2CI%2CL%2Cz%2CN%29%3Ad%3Dh%28z%29%2F%28h%28z%29-h%28N%29%29%3A%C3%A7I%20%C3%B23%28o%25%2Cx%2Bd*I%2CK%2Cw%29%C6%8B%C3%B23%28o%25%2Cx%2CK%2Bd*L%2Cw%29%5Cn%C3%A1%5Cn%C3%9D%C3%B2A%28o%25%2CX%2CY%29%3A%C3%B23%28o%25%2CX%2CY%2Cl%28X%2CY%29%29%3A%C3%A1%5Cn%C3%9D%C3%B23%28o%25%2CX%2CY%2CZ%29%3A%C3%B0o%25%2C640%2B%28X-Y%29*D%25%2C850-%28X%2BY%29*E%25%2BZ*z%25%3A%C3%A1%22%7D). It's a long URL, so might only work in some browsers.

In the accompanying article, it is explained that due to the limits of the BBC's memory, the maximum grid size is just 10x10. Another listing was given for the Acorn Archimedes that could have much higher resolution and colour. I wanted to see if it was possible to bring the higher resolution to the BBC version.

Without a deadline for a magazine release and the benefit of modern tools, it is possible to explore many different enhancements. I made various minor improvements to the algorithms but the biggest was to switch away from generating the whole landscape data in memory before rendering the landscape. Instead, the random fault data is saved into memory and a the landscape height calculated one row at a time, avoiding the quadratic scaling memory usage of the original approach.

Having made a BBC Basic program that could now scale much higher, it was time to deal with the speed. The original program took about 50 seconds to draw a 10x10 landscape. Moving up to 40x40 with the new approach left it taking around 5 minutes. Not very satisfying! So I decided to learn 6502 assembly code, initially thinking to just convert the slowest parts of the code but in the end converting the marjority of it, after generating the random fault data in Basic. Now we can generate large 40x40 landscapes in around 30-35 seconds, faster than the original but with 16 times more patches!

Try out the new enhanced version via [https://bbc.godbolt.org/]([https://bbc.godbolt.org/](https://bbc.godbolt.org/?disc1=https://tommy9.github.io/Triangles/assets/Landscape.ssd&autoboot)https://bbc.godbolt.org/?disc1=https://tommy9.github.io/Triangles/assets/Landscape.ssd&autoboot).
