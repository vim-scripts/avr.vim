" Vim syntax file
" Language:     Atmlel AVR Assembler (Atmel microcontrollers)
" Maintainer:   Kirill Frolov <fk0@fk0.pp.ru>
" Last Change:  2005-01-15T20:11:27+0300
" URL:          http://fk0.pp.ru/avr.vim
" Revision:     0.2

" Message for users:
" 
" C-preprocessor directives and extra directives of avra assembler
" supported too (see http://www.omegav.ntnu.no/~jonah/el/avra.html).
"
" Please send me patches.
" 

" {{{ For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
" }}}

" {{{ Identifiers, numbers, strings, comments, operators...

syn case match
syn keyword avrTodo NOTE TODO FIXME XXX contained
syn case ignore

" any valid identifier
syn match avrIdentifier "[a-z_][a-z0-9_]*"

" valid label
syn match avrLabel      "^[A-Z_][A-Z0-9_]*:"
" me=e-1

" one character
syn match avrASCII      "'.'"

" numbers:
" octal
syn match avrNumber     "\<0[0-7]*\>"
syn match avrNumber     "\<[0-7]\+[oO]\>"
" decimal
syn match avrNumber     "\<[1-9][0-9]*\>"
" hexadecimal
syn match avrNumber     "\<0[Xx][0-9a-fA-F]\+\>"
syn match avrNumber     "\<[0-9][0-9a-fA-F]*[Hh]\>"
syn match avrNumber     "\$[0-9a-fA-F]\+\>"
" binary
syn match avrNumber     "\<0[Bb][01]\+\>"
syn match avrNumber     "\<[0-1]\+[bB]\>"

" string in double quotes
syn region avrString    start=+"+ end=+"+

" comments with special marks
syn match avrComment    ";.*" contains=avrTodo

" registers r0..r31
syn match avrRegister   "\<[Rr]\(30\|31\|[0-2][0-9]\|[0-9]\)\>"

" arithmetic operators
syn keyword avrOperator	! ~ + - * / >> << < <= > >= == != & ^ \| && \|\|

" }}}

" {{{ instruction op-codes

syn case ignore
" unconditional branches
syn keyword avrOpcode   rjmp ijmp eijmp jmp rcall eicall call ret reti
" conditional branches, by status flags
syn keyword avrOpcode   brbs brbc 
syn keyword avrOpcode   brcc brcs brvs brvc brmi brpl breq brne brhs brhc
syn keyword avrOpcode   brts brtc brie brid
" conditional branches, by arithmetic expression
syn keyword avrOpcode   brlo brlt brge brsh
" conditional skip
syn keyword avrOpcode   cpse sbrc sbrs sbic sbis
" arithmetic, signle operand
syn keyword avrOpcode   com neg inc dec tst clr ser
" arithmetic, two operands
syn keyword avrOpcode   add adc adiw sub subi sbc sbci sbiw   cp cpc cpi
syn keyword avrOpcode   and andi or ori eor sbr cbr 
syn keyword avrOpcode   mul muls mulsu fmul fmuls fmulsu
" load instructions
syn keyword avrOpcode   mov movw ldi lds ld ldd sts st std lpm elpm spm espm
" io
syn keyword avrOpcode   in out
" stack manipulations
syn keyword avrOpcode   push pop
" shift instructions
syn keyword avrOpcode   lsl lsr rol ror asr swap
" bit set instructions
syn keyword avrOpcode   bset bclr sbi cbi bst bld sec clc sen cln sez clz
syn keyword avrOpcode   sei cli ses cls sev clv set clt seh clh
" special instructions
syn keyword avrOpcode   nop sleep wdr

" }}}

" {{{ assembler special directives 

" avrasm special directives
" including specific to avra assembler and C preprocessor directives
syn match avrDirective "\.byte"
syn match avrDirective "\.dw"
syn match avrDirective "\.db"
syn match avrDirective "\.device\>"
syn match avrDirective "\.org\>"
syn match avrDirective "\.cseg\>"
syn match avrDirective "\.dseg\>"
syn match avrDirective "\.eseg\>"

syn keyword avrFunction  HIGH LOW BYTE2 BYTE3 BYTE4 LWRD HWRD PAGE EXP2 LOG2

syn match avrMacro     "\.macro\>"
syn match avrMacro     "\.endm\>"
syn match avrMacro     "\.endmacro\>"

" C preprocessor:
syn match avrPreCondit "#if\>"
syn match avrPreCondit "#ifdef\>"
syn match avrPreCondit "#else\>"
syn match avrPreCondit "#endif\>"
" avra specific:
syn match avrPreCondit "\.ifdef\>"
syn match avrPreCondit "\.ifndef\>"
syn match avrPreCondit "\.if\>"
syn match avrPreCondit "\.else\>"
syn match avrPreCondit "\.endif\>"

syn match avrInclude   "\.include\>"
" C preprocessor:
syn match avrInclude   "#include\>"

syn match avrDefine    "\.set\>"
syn match avrDefine    "\.def\>"
syn match avrDefine    "\.equ\>"
" C preprocessor:
syn match avrDefine    "#define\>"
syn match avrDefine    "#undef\>"
" avra specific:
syn match avrDefine    "\.define\>"
syn match avrDefine    "\.undef\>"

syn match avrPreProc   "\.list\>"
syn match avrPreProc   "\.nolist\>"
syn match avrPreProc   "\.listmac\>"

syn match avrPreProc   "\.exit\>"

" avra specific
syn match avrPreProc   "\.message\>"
syn match avrPreProc   "\.warning\>"
syn match avrPreProc   "\.error\>"

" }}}

" {{{ IO register names, bit names, CPU register names

syn case match
" IO registers names
syn keyword avrIOReg    SREG SPH SPL GIMSK GIFR TIMSK TIFR MCUCR MCUSR
syn keyword avrIOReg    TCCR0 TCNT0 TCCR1A TCCR1B TCNT1H TCNT1L 
syn keyword avrIOReg    OCR1AH OCR1AL OCR1BH OCR1BL ICR1H ICR1L
syn keyword avrIOReg    TCCR2 TCNT2 OCR2 ASSR WDTCR
syn keyword avrIOReg    EEARH EEARL EEDR EECR
syn keyword avrIOReg    SPDR SPSR SPCR
syn keyword avrIOReg    UDR USR UCR UBRR UBRRH UBRRL UCSRB UCSRA
syn keyword avrIOReg    ACSR ADMUX ADCSR ADCH ADCL
syn keyword avrIOReg	SP
" IO ports
syn match avrIOReg      "\<PORT[ABCDEF]\>"
syn match avrIOReg      "\<DDR[ABCDEF]\>"
syn match avrIOReg      "\<PIN[ABCDEF]\>"



" IO bit names
syn match avrIOBit    "\<SP[0-9]\>"
syn keyword avrIOBit    INT1 INT0 INTF1 INTF0
syn keyword avrIOBit    OCIE2 TOIE2 TICIE1 OCIE1A OCIE1B TOIE1 TOIE0
syn keyword avrIOBit    OCF2 TOV2 ICF1 OCF1A OCF1B TOV1 TOV0
syn keyword avrIOBit    SE SM1 SM0 ISC11 ISC10 ISC01 ISC00
syn keyword avrIOBit    EXTRF PORF   CS01 CS02 CS00
syn keyword avrIOBit    COM1A1 COM1A0 COM1B1 COM1B0  PWM11 PWM10
syn keyword avrIOBit    ICNC1 ICES1  CTC1 CS12 CS11 CS10
syn keyword avrIOBit    PWM2 COM21 COM20 CTC2 CS22 CS21 CS20
syn keyword avrIOBit    AS2 TCN2UB OCR2UB TCR2UB
syn keyword avrIOBit    WDTOE WDE WDP2 WDP1 WDP0
syn match avrIOBit      "\<EEAR[0-7]\>"
syn keyword avrIOBit    EERIE EEMWE EEWE EERE
syn match avrIOBit      "\<PORT[ABCDEF][0-7]\>"
syn match avrIOBit      "\<P[ABCDEF][0-7]\>"
syn match avrIOBit      "\<DD[ABCDEF][0-7]\>"
syn match avrIOBit      "\<PIN[ABCDEF][0-7]\>"
syn keyword avrIOBit    SPIF WCOL  SPIE SPE DORD MSTR CPOL CPHA SPR1 SPR0
syn keyword avrIOBit    RXC TXC UDRE FE OR
syn keyword avrIOBit    RXCIE TXCIE UDRIE RXEN TXEN CHR9 RXB8 TXB8
syn keyword avrIOBit    ACD AC0 ACI ACIE ACIC ACIS1 ACIS0 MUX2 MUX1 MUX0
syn keyword avrIOBit    ADEN ADSC ADFR ADIF ADIE ADPS2 ADPS1 ADPS0
syn match avrIOBit      "\<ADC[0-9]\>"

" Status Register SREG\: C, Z, N, V, S, H, T, I
syn keyword avrFlags    C Z N V S H T I
" special registers
syn keyword avrRegSpec  X Y Z
"syn match avrRegSpec    "\<\([XYZ][-+]\|[-+][XYZ]\)\>"

" }}}



" {{{ EDIT THIS FOLD TO DEFINE HIGHLIGHT SCHEME MORE PRECISELY

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_avr_syntax_inits")
  if version < 508
    let did_avr_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif


  HiLink avrTodo               Todo
  HiLink avrComment            Comment

  HiLink avrLabel              Label
  HiLink avrFunction           Function
"  HiLink avrSpecLabel          Repeat
  HiLink avrString             String
  HiLink avrASCII              Character
  HiLink avrNumber             Number

  HiLink avrOpcode             Statement
"  HiLink avrBranch             Conditional
"  HiLink avrLoop               Repeat
"  HiLink avrLoadAddr           Operator
"  HiLink avrSpecOp             Exception
  HiLink avrOperator		Operator


"  HiLink avrConst              Constant
  HiLink avrIOReg              Structure
  HiLink avrFlags              Special
  HiLink avrRegister           Type
  HiLink avrRegSpec            Type
  HiLink avrIOBit              Special

"  HiLink avrSpecIdent          Function
"  HiLink avrIdentifier         Identifier

  HiLink avrDirective          Keyword
  HiLink avrPreProc            PreProc
  HiLink avrPreCondit          PreCondit
  HiLink avrInclude            Include
  HiLink avrMacro              Macro
  HiLink avrDefine             Define

  delcommand HiLink
endif

" }}}



let b:current_syntax = "avr"

" vim: ts=8 syntax=vim foldmethod=marker foldmarker={{{,}}}:
