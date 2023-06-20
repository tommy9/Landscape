oswrch=&FFEE

\\ Define some zp locations
ORG &70

\ data for the faults
.numfaults      SKIP 1
.faultNo        SKIP 1 \ index into the fault arrays
.component1addr SKIP 2 \ address of the first component of the determinant calculation array
.dxArray        SKIP 2 \ address of the dx array
.dyArray        SKIP 2 \ address of the dy array
.delta          SKIP 2 \ address of the fault delta array
\ values used in the determinant
.fac1			SKIP 2 \ stores the component1 value precomputed in BASIC
.fac2			SKIP 2 \ output by the multiplication routine
.dx      		SKIP 1
.dy     		SKIP 1
.cx             SKIP 1
.cy      		SKIP 1
\ values used in multiplication routine
.num1lo         SKIP 1
.num1hi         SKIP 1
.num2           SKIP 1
\ accumulated delta
.currentDelta   SKIP 2
\ values used for get line routine
.gridsize       SKIP 1
.resultsAddr1   SKIP 2 \ address of the results array (l0 or l1 in BASIC)
.resultsAddr2   SKIP 2 \ address of the other results array (l0 or l1 in BASIC)
.sign           SKIP 1 \ sign of the result
\ divide fac1 by fac2 and return result in result. rename for clarity
dividend = fac1
divisor = fac2
result = dividend ;save memory by reusing dividend to store the result
adjustx = dx ; for PROCmid passing of information to PROC3d
adjusty = dy

ORG &2400

.start
.setup
{
    \ Load data from CALL parameters - called once at start of program
    \ Parameters:
    \ - number of faults (1 byte)
    \ - gridsize (1 byte)
    \ - address of array of determinant components (2 bytes per fault)
    \ - address of array of dx values (1 byte per fault)
    \ - address of array of dy values (1 byte per fault)
    \ - address of array of delta values (1 byte per fault)
    \
    \ Example: CALL &2E00, ?numfaults, ?component1addr, ?dxArray, ?dyArray, ?delta
    \ use of the ? indirect operator provides the two byte address of the variable directly
    \ that is great for the addresses as we just store those in zp locations
    \ but for the number of faults we need to store the value, not the address
    
    \ numfaults
    LDA &0601 \ just the low byte as numfaults is 8 bit
    STA numfaults

    \ gridsize
    LDA &0604  \ just the low byte as gridsize is 8 bit
    STA gridsize

    JMP calchscale \ skip over local variable memory allocation

    \ local variables
    .*hscale SKIP 2 \ will only need low byte as result almost certainly < 256
  
    \ calculate the hscale value as 320 DIV gridsize
    .calchscale
    {
        \ setup
        LDA #LO(320)
        STA dividend
        LDA #HI(320)
        STA dividend+1
        LDA gridsize
        STA divisor
        LDA #0
        STA divisor+1

        JSR divide16by16
        LDA result
        STA hscale
        LDA result+1
        STA hscale+1
    }

    \ component1addr
    LDA &0607
    STA component1addr
    LDA &0608
    STA component1addr+1

    \ dxArray
    LDA &060A
    STA dxArray
    LDA &060B
    STA dxArray+1

    \ dyArray
    LDA &060D
    STA dyArray
    LDA &060E
    STA dyArray+1

    \ delta
    LDA &0610
    STA delta
    LDA &0611
    STA delta+1

    RTS
}

.remainder  SKIP 2

.divide16by16
{
    .negativecheck
        \ check of both negative and reverse signs (can only divide same sign, so could just check one)
        LDA divisor+1
        BPL setup
        LDA #0
        SEC
        SBC divisor
        STA divisor
        LDA #0
        SBC divisor+1
        STA divisor+1
        LDA #0
        SEC
        SBC dividend
        STA dividend
        LDA #0
        SBC dividend+1
        STA dividend+1

    .setup
        LDA #0
        STA remainder
        STA remainder+1
        LDX #16	        ;repeat for each bit: ...

    .divloop
        ASL dividend	;dividend lb & hb*2, msb -> Carry
        ROL dividend+1	
        ROL remainder	;remainder lb & hb * 2 + msb from carry
        ROL remainder+1
        LDA remainder
        SEC
        SBC divisor	;substract divisor to see if it fits in
        TAY	        ;lb result -> Y, for we may need it later
        LDA remainder+1
        SBC divisor+1
        BCC skip	;if carry=0 then divisor didn't fit in yet

        STA remainder+1	;else save substraction result as new remainder,
        STY remainder	
        INC result	;and INCrement result because divisor fits in 1 time

    .skip
        DEX
        BNE divloop	
        RTS
}

\ main function for getting the landscape height of one row
.doline
    \ cx and results array address set by the caller

    \ process parameters
    \ cx
    \LDA &0601 \ just the low byte as cx is 8 bit
    \STA cx

    \ result array
    \LDA &0604
    \STA resultsAddr1
    \LDA &0605
    \STA resultsAddr1+1

    \loop through all the points (y loop)
    LDY gridsize
    STY cy
    .yloop
        LDX cx \ restore as gets obliterated in the subroutines
        LDY cy
        JSR doCheck \ (result in currentDelta)
        \copy result to results array
        LDA cy
        ASL A
        TAY
        LDA currentDelta
        STA (resultsAddr1),Y
        LDA currentDelta+1
        INY
        STA (resultsAddr1),Y
        DEC cy
    BPL yloop
    RTS

.doCheck \\calculate height at pos cx,cy
    LDA numfaults
    STA faultNo \ faultNo is used as an array index, so start at numfaults-1
    DEC faultNo
    LDA #0 \ clear result
    STA currentDelta
    STA currentDelta+1
    .faultLoop
        LDA faultNo
        JSR checkfault \ accumulator holds high byte of result, so can check if positive or negative easily
        BMI below
        \ add signed 8bit delta array value to current delta
        LDY faultNo
        LDX #0
        CLC
        LDA (delta),Y
        BPL positivedelta
        DEX
        .positivedelta
        ADC currentDelta
        STA currentDelta
        TXA
        ADC currentDelta+1
        STA currentDelta+1

        .below \ no change to current delta at this position
        DEC faultNo
    BPL faultLoop
    RTS
.checkfault
    \\ Accumulator holds the index into the fault arrays
    \\ copy data into zp locations for the determinant
    TAY \ want to use Y offset of the array entry points
    LDA (dxArray),Y
    STA dx
    LDA (dyArray),Y
    STA dy
    TYA \ return to the faultNo
    ASL A \ need to multiply by 2 because the array entries are 2 bytes
    TAY
    LDA (component1addr),Y
    STA fac1
    INY
    LDA (component1addr),Y
    STA fac1+1
    JSR determinant
    RTS
.determinant
{
\\ Inputs: 
\\ - precomputed factor (dx*ay-dy*ax). Signed 16 bit
\\ - dx. Signed 8 bit <= S%
\\ - dy. Signed 8 bit <= S%
\\ - cx. Unsigned 8 bit <= S%
\\ - cy. Unsigned 8 bit <= S%

\\ Outputs:
\\ - indicator if a point is above or below the line. 0 if below, 1 if above. Unsigned 8 bit <= S%

\\ Calculation:
\\ fac1 - dx*cy + dy*cx <= 0
\\
\\ order of calculation:
\\ 1. dx*cy = "fac2"
\\ 2. subtract fac2 from fac1
\\ 3. dy*cx = "fac2"
\\ 4. add fac2 to fac1
\\ Result is check of result is negative or not (could be in sign bit or separate register/address)

    \1. dx*cy
    LDX #0
    LDA dx
    STA num1lo
    BPL dxpositive
    DEX
    .dxpositive
    STX num1hi
    LDA cy
    STA num2
    JSR multiply8to16
    \2. subtract fac2 from fac1 (16 bit subtraction)
    SEC
    LDA fac1
    SBC fac2
    STA fac1
    LDA fac1+1
    SBC fac2+1
    STA fac1+1
    \3. dy*cx
    LDX #0
    LDA dy
    STA num1lo
    BPL dypositive
    DEX
    .dypositive
    STX num1hi
    LDA cx
    STA num2
    JSR multiply8to16
    \4. add fac2 to fac1 (16 bit addition)
    CLC
    LDA fac1
    ADC fac2
    STA fac1
    LDA fac1+1
    ADC fac2+1
    STA fac1+1
    RTS
\Multiplies "num1" by "num2" and stores result in .A (low byte, also in .X) and .Y (high byte)
\uses extra zp var "num1hi" which should hold the high byte of num1

    .^multiply8to16
    {
        LDA #$00
        TAY
        \ STY num1hi 
        BEQ enterLoop
    .doAdd
        CLC
        ADC num1lo
        TAX
        TYA
        ADC num1hi
        TAY
        TXA
    .mulLoop
        ASL num1lo
        ROL num1hi
    .enterLoop  ; accumulating multiply entry point (enter with .A=lo, .Y=hi)
        LSR num2
        BCS doAdd
        BNE mulLoop
    .storeAndReturn
        STA fac2
        STY fac2+1
        RTS
    }
}
.drawlandscape
{
    .saveParams
    {
        \ TEMPORARY: take in 2 addresses from BASIC
        \ keep cx from memory and reuse it

        \ process parameters
        \ low X resultsAddr
        LDA &0601
        STA resultsAddr1
        LDA &0602
        STA resultsAddr1+1

        \ high X resultsAddr
        LDA &0604
        STA resultsAddr2
        LDA &0605
        STA resultsAddr2+1
    }


    JMP setuptemp   \ skip over memory allocation
    \ local variables (unlikely to benefit from zp?)
    .^heights    SKIP 8 \ could directly access landscape array, but this is more readable
    .^water      SKIP 2
    .^colour     SKIP 1
    .^base       SKIP 2
    .setuptemp
    LDA #0
    STA water
    STA water+1
    LDA #1
    STA colour
    LDA #LO(-400)
    STA base
    LDA #HI(-400)
    STA base+1

    JSR PROCsea


\    .testingonly
\    {
\        LDA #11
\        STA gridsize
\        LDA #0
\        STA cx
\        LDA #LO(testingdataRow1)
\        STA resultsAddr1
\        LDA #HI(testingdataRow1)
\        STA resultsAddr1+1
\        LDA #LO(testingdataRow2)
\        STA resultsAddr2
\        LDA #HI(testingdataRow2)
\        STA resultsAddr2+1
\
\        LDA #0
\        STA cx
\        STA cy
\        STA Xcoord
\        STA Ycoord
\        JSR PROCpatch
\
\        LDA #3
\        STA cy
\        STA Ycoord
\        JSR PROCpatch
\
\        LDA #6
\        STA cy
\        STA Ycoord
\        JSR PROCpatch
\
\        LDA #9
\        STA cy
\        STA Ycoord
\        JSR PROCpatch
\    }
\    RTS \ TODO: remove above or make it a flag?

    .mainloop
    {
        \ loop over the x and y coord with cx and cy
        LDX #0
        STX cx
        STX Xcoord

        \call doline(0)
        \LDA cx
        \STA &0601
        \LDA resultsAddr2
        \STA &0604
        \LDA resultsAddr2+1
        \STA &0605
        JSR doline
        LDA #0
        STA adjustx
        STA adjusty

        .xloop
            \ call doline(cx+1)
            INC cx \ temporary as just for calling doline with cx+1
            \INX
            \STX &0601
            \ swap resultsAddr2 and resultsAddr1 and call doline
            LDA resultsAddr2
            PHA
            LDA resultsAddr2+1
            PHA
            LDA resultsAddr1
            STA resultsAddr2
            \STA &0604
            LDA resultsAddr1+1
            STA resultsAddr2+1
            \STA &0605
            PLA
            STA resultsAddr1+1
            PLA
            STA resultsAddr1
            JSR doline
            LDA #0
            STA adjustx
            STA adjusty
            DEC cx \ undo temporary increment
            \ set starting colour based on odd or even row
            LDA cx
            AND #1
            TAY
            INY
            STY colour

            LDY #0
            STY cy
            .yloop
                LDA cy
                STA Ycoord
                \ swap colours
                LDA colour
                EOR #3
                STA colour
                \ draw patch
                JSR PROCpatch
                \ for debugging, add check for key press before moving on
                \LDA #&81
                \LDX #100 \ delay in centiseconds
                \LDY #0 \ high byte of delay
                \JSR &FFF4 \ OSBYTE
                \CPY #&1B \ ESC key
                \BNE notesc
                \RTS \ exit
                \.notesc
                \ check for edge
                LDX cx
                INX
                CPX gridsize
                BNE notedge
                JSR PROCside
                .notedge
                \ next Y
                INC cy
                LDA cy
                CMP gridsize
                BNE yloop
            JSR PROCedge
            \ next X
            INC cx
            LDA cx
            CMP gridsize
            BNE xloop

        RTS
    }


    .PROCpatch \ DEFPROCpatch(I%,J%)
    {
        \ copy height data into local variables
        \ Xcoord and Ycoord already set up
        \ note that furthest away (lower X) is in resultsAddr2 because update resultsAddr1 each loop
        LDA Ycoord
        ASL A
        TAY
        LDA (resultsAddr2),Y    \ heights(0) = landscape(X,Y)
        STA heights
        LDA (resultsAddr1),Y    \ heights(1) = landscape(X+1,Y)
        STA heights+2
        INY
        LDA (resultsAddr2),Y
        STA heights+1
        LDA (resultsAddr1),Y
        STA heights+3
        INY
        LDA (resultsAddr1),Y    \ heights(2) = landscape(X+1,Y+1)
        STA heights+4
        LDA (resultsAddr2),Y    \ heights(3) = landscape(X,Y+1)
        STA heights+6
        INY
        LDA (resultsAddr1),Y
        STA heights+5
        LDA (resultsAddr2),Y
        STA heights+7

        \ GCOL 0,colour === VDU 18,0,colour
        LDA #18: JSR oswrch
        LDA #0: JSR oswrch
        LDA colour: JSR oswrch

        \ select patch type
        \ for each height, if negative, first bit is set
        LDA #0
        LDX heights+1
        BPL height1positive
        ORA #&08
        .height1positive
        LDX heights+3
        BPL height2positive
        ORA #&04
        .height2positive
        LDX heights+5
        BPL height3positive
        ORA #&02
        .height3positive
        LDX heights+7
        BPL height4positive
        ORA #&01
        .height4positive

        CMP #&00
        BNE check1
        JSR FNaaaa: RTS
        .check1
        CMP #&01
        BNE check2
        JSR FNaaab: RTS
        .check2
        CMP #&02
        BNE check3
        JSR FNaaba: RTS
        .check3
        CMP #&03
        BNE check4
        JSR FNaabb: RTS
        .check4
        CMP #&04
        BNE check5
        JSR FNabaa: RTS
        .check5
        CMP #&05
        BNE check6
        JSR FNabab: RTS
        .check6
        CMP #&06
        BNE check7
        JSR FNabba: RTS
        .check7
        CMP #&07
        BNE check8
        JSR FNabbb: RTS
        .check8
        CMP #&08
        BNE check9
        JSR FNbaaa: RTS
        .check9
        CMP #&09
        BNE check10
        JSR FNbaab: RTS
        .check10
        CMP #&0A
        BNE check11
        JSR FNbaba: RTS
        .check11
        CMP #&0B
        BNE check12
        JSR FNbabb: RTS
        .check12
        CMP #&0C
        BNE check13
        JSR FNbbaa: RTS
        .check13
        CMP #&0D
        BNE check14
        JSR FNbbab: RTS
        .check14
        CMP #&0E
        BNE check15
        JSR FNbbba: RTS
        .check15
        RTS \ don't need to plot if all points below water level
    }

    .PROCside
    {
        \ GCOL 0,colour === VDU 18,0,colour
        LDA #18: JSR oswrch
        LDA #0: JSR oswrch
        LDA colour: JSR oswrch

        \ PROC3d(4,S%,J%,base)
        LDA #4: STA plottype
        LDA gridsize: STA Xcoord
        LDA cy: STA Ycoord
        LDA base: STA Zcoord
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \ PROC3d(4,S%,J%+1,base)
        INC Ycoord
        LDA base: STA Zcoord \ have to reset Zcoord as PROC3d modifies it
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \ PROC3dd(85,S%,J%,1)
        LDA #85: STA plottype
        DEC Ycoord
        LDA #1: STA corner
        JSR PROC3dd

        \ PROC3dd(85,S%,J%+1,2)
        INC Ycoord
        LDA #2: STA corner
        JSR PROC3dd

        RTS
    }

    .PROCedge
    {
        \ GCOL 0,colour === VDU 18,0,colour
        LDA #18: JSR oswrch
        LDA #0: JSR oswrch
        LDA colour: JSR oswrch

        \ PROC3d(4,I%,S%,base)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA gridsize: STA Ycoord
        LDA base: STA Zcoord
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \ PROC3d(4,I%+1,S%,base)
        INC Xcoord
        LDA base: STA Zcoord \ have to reset Zcoord as PROC3d modifies it
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \ PROC3dd(85,I%,S%,3)
        LDA #85: STA plottype
        DEC Xcoord
        LDA #3: STA corner
        JSR PROC3dd

        \ PROC3dd(85,I%+1,S%,2)
        INC Xcoord
        LDA #2: STA corner
        JSR PROC3dd

        RTS
    }

    .PROCsea
    {
        \GCOL 0,3 = blue
        LDA #18: JSR oswrch
        LDA #0: JSR oswrch
        LDA #3: JSR oswrch

        \PROC3d(4,0,0,water)
        LDA #4: STA plottype
        LDA #0: STA Xcoord
        LDA #0: STA Ycoord
        LDA water: STA Zcoord
        LDA water+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(4,S%,0,water)
        LDA gridsize: STA Xcoord
        LDA water: STA Zcoord
        LDA water+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(85,0,S%,water)
        LDA #85: STA plottype
        LDA #0: STA Xcoord
        LDA gridsize: STA Ycoord
        LDA water: STA Zcoord
        LDA water+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(85,S%,S%,water)
        LDA gridsize: STA Xcoord
        LDA water: STA Zcoord
        LDA water+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(85,0,S%,base)
        LDA #0: STA Xcoord
        LDA base: STA Zcoord
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(85,S%,S%,base)
        LDA gridsize: STA Xcoord
        LDA base: STA Zcoord \ have to reset Zcoord as PROC3d modifies it
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(4,S%,0,base)
        LDA #4: STA plottype
        LDA #0: STA Ycoord
        LDA base: STA Zcoord \ have to reset Zcoord as PROC3d modifies it
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(4,S%,S%,base)
        LDA gridsize: STA Ycoord
        LDA base: STA Zcoord \ have to reset Zcoord as PROC3d modifies it
        LDA base+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(85,S%,0,water)
        LDA #0: STA Ycoord
        LDA #85: STA plottype
        LDA water: STA Zcoord
        LDA water+1: STA Zcoord+1
        JSR PROC3d

        \PROC3d(85,S%,S%,water)
        LDA gridsize: STA Ycoord
        LDA water: STA Zcoord
        LDA water+1: STA Zcoord+1
        JSR PROC3d

        RTS
    }

    .FNaaaa \ all points above water
    {
        \ move to 2 points, then draw triangles to next two points
        \ PROC3dd(4,X%,Y%,0)
        \ PROC3dd(4,X%+1,Y%,1)
        \ PROC3dd(85,X%,Y%+1,3)
        \ PROC3dd(85,X%+1,Y%+1,2)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        LDA #0: STA corner
        JSR PROC3dd

        INC Xcoord
        LDA #1: STA corner
        JSR PROC3dd

        LDA #85: STA plottype
        DEC Xcoord
        INC Ycoord
        LDA #3: STA corner
        JSR PROC3dd

        INC Xcoord
        LDA #2: STA corner
        JSR PROC3dd

        RTS
    }

    .FNaaab
    {
        \ PROC3dd(4,X%,Y%,0)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        LDA #0: STA corner
        JSR PROC3dd

        \ PROC3dd(4,X%+1,Y%,1)
        INC Xcoord
        LDA #1: STA corner
        JSR PROC3dd

        \ PROC3dm(85,3) : will use X%,Y%
        DEC Xcoord
        LDA #85: STA plottype
        LDA #3: STA side1
        JSR PROC3dm

        \ PROC3dd(85,X%+1,Y%+1,2)
        INC Xcoord
        INC Ycoord
        LDA #2: STA side1
        JSR PROC3dd

        \ PROC3dm(85,2) : will use X%,Y%+1
        DEC Xcoord
        LDA #2: STA corner
        JSR PROC3dm

        RTS
    }

    .FNaaba
    {
        \ PROC3dd(4,X%,Y%,0)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        LDA #0: STA corner
        JSR PROC3dd

        \ PROC3dd(4,X%,Y%+1,3)
        INC Ycoord
        LDA #3: STA corner
        JSR PROC3dd

        \ PROC3dd(85,X%+1,Y%,1)
        LDA #85: STA plottype
        INC Xcoord
        DEC Ycoord
        LDA #1: STA corner
        JSR PROC3dd

        \ PROC3dm(85,2) : will use X%,Y%+1
        DEC Xcoord
        INC Ycoord
        LDA #2: STA side1
        JSR PROC3dm

        \ PROC3dm(85,1) : will use X%+1,Y%
        INC Xcoord
        DEC Ycoord
        LDA #1: STA side1
        JSR PROC3dm

        RTS
    }

    .FNaabb
    {
        \ PROC3dd(4,X%,Y%,0)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        LDA #0: STA corner
        JSR PROC3dd

        \ PROC3dd(4,X%+1,Y%,1)
        INC Xcoord
        LDA #1: STA corner
        JSR PROC3dd

        \ PROC3dm(85,3) : will use X%,Y%
        DEC Xcoord
        LDA #85: STA plottype
        LDA #3: STA side1
        JSR PROC3dm

        \ PROC3dm(85,1) : will use X%+1,Y%
        INC Xcoord
        LDA #1: STA side1
        JSR PROC3dm

        RTS
    }

    .FNabaa
    {
        \ PROC3dd(4,X%,Y%+1,3)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        INC Ycoord
        LDA #3: STA corner
        JSR PROC3dd

        \ PROC3dd(4,X%+1,Y%+1,2)
        INC Xcoord
        LDA #2: STA corner
        JSR PROC3dd

        \ PROC3dd(85,X%,Y%,0)
        LDA #85: STA plottype
        DEC Xcoord
        DEC Ycoord
        LDA #0: STA corner
        JSR PROC3dd

        \ PROC3dm(85,1) : will use X%+1,Y%
        INC Xcoord
        LDA #1: STA side1
        JSR PROC3dm

        \ PROC3dm(85,0) : will use X%,Y%
        DEC Xcoord
        LDA #0: STA side1
        JSR PROC3dm

        RTS
    }

    .FNabab
    {
        \ PROCtri(0)
        \ PROCtri(2)
        LDA #0: STA corner
        JSR PROCtri
        LDA #2: STA corner
        JSR PROCtri
        RTS
    }

    .FNabba
    {
        \ PROC3dd(4,X%,Y%,0)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        LDA #0: STA corner
        JSR PROC3dd

        \ PROC3dd(4,X%,Y%+1,3)
        INC Ycoord
        LDA #3: STA corner
        JSR PROC3dd

        \ PROC3dm(85,0) : will use X%,Y%
        DEC Ycoord
        LDA #85: STA plottype
        LDA #0: STA side1
        JSR PROC3dm

        \ PROC3dm(85,2) : will use X%,Y%+1
        INC Ycoord
        LDA #2: STA side1
        JSR PROC3dm

        RTS
    }

    .FNabbb
    {
        \ PROCtri(0)
        LDA #0: STA corner
        JSR PROCtri
        RTS
    }

    .FNbaaa
    {
        \ PROC3dm(4,3)
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        LDA #4: STA plottype
        LDA #3: STA side1
        JSR PROC3dm

        \ PROC3dm(4,0)
        LDA #0: STA side1
        JSR PROC3dm

        \ PROC3dd(85,X%,Y%+1,3)
        INC Ycoord
        LDA #85: STA plottype
        LDA #3: STA corner
        JSR PROC3dd

        \ PROC3dd(85,X%+1,Y%,1)
        INC Xcoord
        DEC Ycoord
        LDA #1: STA corner
        JSR PROC3dd

        \ PROC3dd(85,X%+1,Y%+1,2)
        INC Ycoord
        LDA #2: STA corner
        JSR PROC3dd

        RTS
    }

    .FNbaab
    {
        \ PROC3dd(4,X%+1,Y%+1,2)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        INC Xcoord
        INC Ycoord
        LDA #2: STA corner
        JSR PROC3dd

        \ PROC3dd(4,X%+1,Y%,1)
        DEC Ycoord
        LDA #1: STA corner
        JSR PROC3dd

        \ PROC3dm(85,2)
        INC Ycoord
        DEC Xcoord
        LDA #85: STA plottype
        LDA #2: STA side1
        JSR PROC3dm

        \ PROC3dm(85,0)
        DEC Ycoord
        LDA #0: STA side1
        JSR PROC3dm

        RTS
    }

    .FNbaba
    {
        \ PROCtri(1)
        LDA #1: STA corner
        JSR PROCtri

        \ PROCtri(3)
        LDA #3: STA corner
        JSR PROCtri

        RTS
    }

    .FNbabb
    {
        \ PROCtri(1)
        LDA #1: STA corner
        JSR PROCtri

        RTS
    }

    .FNbbaa
    {
        \ PROC3dd(4,X%+1,Y%+1,2)
        LDA #4: STA plottype
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        INC Xcoord
        INC Ycoord
        LDA #2: STA corner
        JSR PROC3dd

        \ PROC3dd(4,X%,Y%+1,3)
        DEC Xcoord
        LDA #3: STA corner
        JSR PROC3dd

        \ PROC3dm(85,1)
        INC Xcoord
        DEC Ycoord
        LDA #85: STA plottype
        LDA #1: STA side1
        JSR PROC3dm

        \ PROC3dm(85,3)
        DEC Xcoord
        LDA #3: STA side1
        JSR PROC3dm

        RTS
    }

    .FNbbab
    {
        \ PROCtri(2)
        LDA #2: STA corner
        JSR PROCtri

        RTS
    }

    .FNbbba
    {
        \ PROCtri(3)
        LDA #3: STA corner
        JSR PROCtri

        RTS
    }

}

.plottingRoutines
{
    \ parameters to be set by the caller
    \ heavily reused between functions to save copying
    .^plottype  SKIP 1 \ o%. For PLOT plottype, x, y calls
    .^Xcoord    SKIP 1 \ on original 0...S% scale
    .^Ycoord    SKIP 1 \ on original 0...S% scale
    .^Zcoord    SKIP 2 \ absolute height from landscape (inc water)
    .^side1     SKIP 1 \ sides to pay attention to
    .^side2     SKIP 1
    .^corner    SKIP 1 \ for triangles, which corner to plot
    .xax        SKIP 1 \ 0 or 1 for X or Y axis

    .^PROC3dd
    {
        \ sets Zcoord to the height of the point for given corner
        \ DEFPROC3dd(o%,X,Y,c%)
        \ Z=height(c%)+water
        \ PROC3d(o%,X,Y,Z)
        \ ENDPROC
        \ just passing through o%, X and Y. Sets Z
        LDA corner
        ASL A \ 2 byte array heights()
        TAY
        LDA heights,Y
        STA Zcoord
        INY
        LDA heights,Y \ could just skip INY and reference heights+1 but this is clearer
        STA Zcoord+1
        JSR PROC3d
        RTS
    }


    .^PROCtri
    {
        \ DEFPROCtri(t%)
        \ IF t%=0 PROCtat(X%,Y%,3,0)
        \ IF t%=1 PROCtat(X%+1,Y%,0,1)
        \ IF t%=2 PROCtat(X%+1,Y%+1,1,2)
        \ IF t%=3 PROCtat(X%,Y%+1,2,3)
        \ ENDPROC
        LDA cx: STA Xcoord
        LDA cy: STA Ycoord
        LDA corner
        CMP #0
        BNE not0
        JSR corner0
        RTS
        .not0
        CMP #1
        BNE not1
        JSR corner1
        RTS
        .not1
        CMP #2
        BNE not2
        JSR corner2
        RTS
        .not2
        CMP #3
        BEQ corner3
        RTS

        .corner0
        LDA #3: STA side1
        LDA #0: STA side2
        JSR PROCtat
        RTS

        .corner1
        INC Xcoord
        LDA #0: STA side1
        LDA #1: STA side2
        JSR PROCtat
        RTS

        .corner2
        INC Xcoord
        INC Ycoord
        LDA #1: STA side1
        LDA #2: STA side2
        JSR PROCtat
        RTS

        .corner3
        INC Ycoord
        LDA #2: STA side1
        LDA #3: STA side2
        JSR PROCtat
        RTS
    }

    .^PROCtat
    {
        \ DEFPROCtat(x%,y%,s1%,s2%)
        \ use plottype, side1, side2

        \ PROC3dd(4,x%,y%,s2%)
        LDA #4: STA plottype
        LDA side2
        STA corner
        PHA \ save side2
        JSR PROC3dd

        \ PROC3dm(4,s1%) - side1 already set
        JSR PROC3dm

        \ PROC3dm(85,s2%)
        LDA #85: STA plottype
        PLA \ restore side2
        \ LDA side2 \ side 2 is being changed by PROC3dm
        STA side1
        JSR PROC3dm

        RTS
    }

    .^PROC3dm
    {
        \ call PROCmid to do the real work
        \ DEFPROC3dm(o%,s%)
        \ IF s%=0 PROCmid(o%,X%,Y%,1,0,1)
        \ IF s%=1 PROCmid(o%,X%+1,Y%,0,1,2)
        \ IF s%=2 PROCmid(o%,X%,Y%+1,1,3,2)
        \ IF s%=3 PROCmid(o%,X%,Y%,0,0,3)
        \ ENDPROC
        \ use side1
        LDX cx: STX Xcoord
        LDY cy: STY Ycoord
        LDA water
        STA Zcoord
        LDA water+1
        STA Zcoord+1
        LDA side1
        CMP #0
        BNE not0
        JSR sideis0
        RTS
        .not0
        CMP #1
        BNE not1
        JSR sideis1
        RTS
        .not1
        CMP #2
        BNE not2
        JSR sideis2
        RTS
        .not2
        CMP #3
        BEQ sideis3
        RTS \ Should not end up here

        .sideis0
        LDA #1: STA xax
        LDA #0: STA side1
        LDA #1: STA side2
        JSR PROCmid
        RTS

        .sideis1
        INC Xcoord
        LDA #0: STA xax
        LDA #1: STA side1
        LDA #2: STA side2
        JSR PROCmid
        RTS

        .sideis2
        INC Ycoord
        LDA #1: STA xax
        LDA #3: STA side1
        LDA #2: STA side2
        JSR PROCmid
        RTS

        .sideis3
        LDA #0: STA xax
        LDA #0: STA side1
        LDA #3: STA side2
        JSR PROCmid
        RTS
    }

    .PROCmid
    {
        \ gets the proportion of the way through the line and multiplies by the horizontal scale
        \ which will be either hscale or hscale*2 depending on the X or Y direction
        \ then adds to the starting position through an extra variable for PROC3d to pick up
        \ DEFPROCmid(o%,xs,ys,xax%,zs,ze)
        \ d=height(zs)/(height(zs)-height(ze))
        \ IF xax% PROC3d(o%,xs+d,ys,water) ELSE PROC3d(o%,xs,ys+d,water)
        \ ENDPROC
        \ use plottype, Xcoord, Ycoord, xax, side1, side2
        
        \ will need a division routine for 16 bit numbers
        \ step 1: multiply height(side1) by hscale x 2 to get in fac1
        \ step 2: subtract height(side2) from height(side1) and put in fac2
        \ step 3: divide result of step 1 by the result of step 2
        \ (worry about rounding? should just need to add 0.5 of something?)

        \ assume adjustment applies to y and switch later if that is the case
        LDA #0: STA adjustx
        LDA #1: STA adjusty
        
        \ step 1
        LDA side1
        ASL A \ 2 byte array heights()
        TAY
        LDA heights,Y
        STA num1lo
        INY
        LDA heights,Y
        STA num1hi
        LDA hscale
        ASL A
        LDX xax
        BEQ yside
        INC adjustx
        DEC adjusty
        .yside
        STA num2
        JSR multiply8to16 \results are in fac2 so copy to fac1
        LDA fac2
        STA fac1
        LDA fac2+1
        STA fac1+1
        \ step 2
        LDA side1
        ASL A \ 2 byte array heights()
        TAY
        LDA heights,Y
        STA fac2
        LDA heights+1,Y
        STA fac2+1

        LDA side2
        ASL A
        TAY
        LDA fac2
        SEC
        SBC heights,Y
        STA fac2
        LDA fac2+1
        SBC heights+1,Y
        STA fac2+1
        \ step 3
        JSR divide16by16
        \ plot triangle
        JSR PROC3d
        RTS
    }

    .screenpos SKIP 2
    .^PROC3d
    {
        \ works on screen coordinates 1280 x 1024
        LDA #25: JSR oswrch \ VDU 25 === PLOT
        LDA plottype: JSR oswrch
        \ ** Calculate the x position **
        \ 640+(X-Y)*hscale%*2 (+adjustment to midline)
        LDA #LO(640)
        STA screenpos
        LDA #HI(640)
        STA screenpos+1
        \ midline adjustment
        LDA adjustx
        BEQ noadjustx1
        LDA result
        CLC
        ADC screenpos
        STA screenpos
        LDA result+1
        ADC screenpos+1
        STA screenpos+1
        .noadjustx1
        LDA adjusty
        BEQ noadjusty1
        LDA screenpos
        SEC
        SBC result
        STA screenpos
        LDA screenpos+1
        SBC result+1
        STA screenpos+1
        .noadjusty1
        LSR result+1 \ divide by 2 so it gets the right scaling for adjusting the Y coordinate
        ROR result
        \ put (X-Y) in num1 (num1lo and num1hi)
        \ put hscale * 2 in num2 (safely assume 1 byte)
        LDA hscale
        ASL A
        STA num2
        LDX #0
        LDA Xcoord
        SEC
        SBC Ycoord
        STA num1lo
        BPL num1Xpositive
        INX \ num1 is negative so remember that
        EOR #$FF
        CLC
        ADC #1 \ 2's complement
        STA num1lo
        .num1Xpositive
        STX sign
        LDA #0
        STA num1hi
        JSR multiply8to16 \ results in A and Y
        \ change sign of result if (X-Y) was negative
        LDX sign
        BEQ num1Xpositive2
        \ subtract fac2 from 0
        LDA #0
        SEC
        SBC fac2
        STA fac2
        LDA #0
        SBC fac2+1
        STA fac2+1
        .num1Xpositive2
        \ add screenpos to fac2
        LDA fac2
        CLC
        ADC screenpos
        STA screenpos
        LDA fac2+1
        ADC screenpos+1
        STA screenpos+1
        LDA screenpos: JSR oswrch
        LDA screenpos+1: JSR oswrch

        \ ** Calculate the y position **
        \ 850-(X+Y)*hscale%+Z*zscale (zscale just ROR,ROR i.e. divide by 4?)
        LDA #LO(850)
        STA screenpos
        LDA #HI(850)
        STA screenpos+1
        \ midline adjustment
        LDA adjusty
        ORA adjustx
        BEQ noadjusty2
        LDA screenpos
        SEC
        SBC result
        STA screenpos
        LDA screenpos+1
        SBC result+1
        STA screenpos+1
        .noadjusty2
        \ put (X+Y) in num1 (num1lo and num1hi)
        \ put hscale in num2 (safely assume 1 byte)
        LDA hscale
        STA num2
        LDA Xcoord
        CLC
        ADC Ycoord
        STA num1lo
        LDA #0
        STA num1hi
        JSR multiply8to16 \ results in fac2 (also A and Y)
        \ subtract num1 from screenpos
        LDA screenpos
        SEC
        SBC fac2
        STA screenpos
        LDA screenpos+1
        SBC fac2+1
        STA screenpos+1
        \ apply zscale to Zcoord (divide by 4 by 2 RORs)
        LDA Zcoord+1
        CLC
        BPL zpositive1
        SEC
        .zpositive1
        ROR Zcoord+1
        ROR Zcoord
        \ second divide by 2
        LDA Zcoord+1
        CLC
        BPL zpositive2
        SEC
        .zpositive2
        ROR Zcoord+1
        ROR Zcoord
        \ third divide by 2
        LDA Zcoord+1
        CLC
        BPL zpositive3
        SEC
        .zpositive3
        ROR Zcoord+1
        ROR Zcoord        
        \ add Z*zscale to screenpos
        CLC
        LDA screenpos
        ADC Zcoord
        STA screenpos
        LDA screenpos+1
        ADC Zcoord+1
        STA screenpos+1
        LDA screenpos: JSR oswrch
        LDA screenpos+1: JSR oswrch
        \ clear adjustment indicators
        LDA #0
        STA adjustx
        STA adjusty
        RTS
    }
}
\.testingdataRow1
\ EQUW 100, 100, 0, 100, 100, 0, 100, 100, 0, 100, 100
\ EQUW 0, 0, 0, 100, -50, 0, -20, 40, 0, -20, -20
\ EQUW -20, 40, 0, -20, -20, 0, 40, -20, 0, -10, -10
\EQUW -20, 40, 0, 40, -20, 0, 40, -20, 0, -10, -10
\.testingdataRow2
\EQUW 40, -20, 0, -20, 40, 0, -20, -20, 0, 40, -10
\ EQUW -20, -20, 0, -20, 40, 0, -20, -20, 0, 40, -10
\ EQUW 100, 100, 0, 100, -50, 0, -50, 100, 0, -50, -50
\ EQUW 0, 0, 0, 100, 100, 0, 40, 40, 0, 40, 40

.end

SAVE "CORE", start, end
PUTBASIC "BeebEmbiggened.bbc", "LAND"
PUTFILE "!Boot.txt", "!Boot", &FFFFFF
PRINT "Address of doline function: ", ~doline
PRINT "Address of drawlandscape: ", ~drawlandscape