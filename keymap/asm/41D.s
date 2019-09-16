; Name:   Swedish
; Locale: sv-SE
; KLID:   41d
;
; PETSCII characters reachable on a C64 keyboard that are not reachable with this layout:
; chars: 'π'
; codes: RUN/STOP SHIFT_DISABLE TEXT_MODE CURSOR_DOWN HOME 
; graph: '\xa6\xa9\xba'
; ASCII characters reachable with this layout on Windows but not covered by PETSCII:
; '\x1b\x1c\x1dABCFGHIJKLMNOPQRSTUWXYZ\_{|}~¤§¨´µ½ÄÅÖäåö€'

.segment "KBDMETA"

	.byte "SV-SE", 0
	.word kbtab_41d_1-13
	.word kbtab_41d_4-13
	.word kbtab_41d_2-13
	.word kbtab_41d_6-13
	.word kbtab_41d_0

.segment "KBDTABLES"

kbtab_41d_0: ; Unshifted
	.byte $00,$00,$88,$87,$86,$85,$89,$00
	.byte $00,$00,$8c,$8b,$8a,$09,$00,$00
	.byte $00,$00,$00,$00,$00,'q','1',$00
	.byte $00,$00,'z','s','a','w','2',$00
	.byte $00,'c','x','d','e','4','3',$00
	.byte $00,' ','v','f','t','r','5',$00
	.byte $00,'n','b','h','g','y','6',$00
	.byte $00,$00,'m','j','u','7','8',$00
	.byte $00,',','k','i','o','0','9',$00
	.byte $00,'.','-','l',$f6,'p','+',$00
	.byte $00,$00,$e4,$00,$e5,$b4,$00,$00
	.byte $00,$00,$0d,$a8,$00,''',$00,$00
	.byte $00,'<',$00,$00,$00,$00,$14,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,',',$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
kbtab_41d_1: ; Shft 
	.byte $18,$00,$00
	.byte $00,$00,$00,$00,$00,'Q','!',$00
	.byte $00,$00,'Z','S','A','W','"',$00
	.byte $00,'C','X','D','E',$a4,'#',$00
	.byte $00,$a0,'V','F','T','R','%',$00
	.byte $00,'N','B','H','G','Y','&',$00
	.byte $00,$00,'M','J','U','/','(',$00
	.byte $00,';','K','I','O','=',')',$00
	.byte $00,':','_','L',$d6,'P','?',$00
	.byte $00,$00,$c4,$00,$c5,'`',$00,$00
	.byte $00,$00,$8d,'^',$00,'*',$00,$00
	.byte $00,'>',$00,$00,$00,$00,$94,$00
kbtab_41d_2: ; Ctrl 
	.byte $18,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$90,$00
	.byte $00,$00,$00,$00,$00,$00,$05,$00
	.byte $00,$00,$00,$00,$00,$9f,$1c,$00
	.byte $00,$a0,$00,$00,$00,$00,$9c,$00
	.byte $00,$00,$00,$00,$00,$00,$1e,$00
	.byte $00,$00,$00,$00,$00,$1f,$9e,$00
	.byte $00,$00,$00,$00,$00,$92,$12,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$1b,$00,$00,$00
	.byte $00,$00,$8d,$1d,$00,$00,$00,$00
	.byte $00,$1c,$00,$00,$00,$00,$94,$00
kbtab_41d_4: ; Alt 
	.byte $18,$00,$00
	.byte $00,$00,$00,$00,$00,$ab,$81,$00
	.byte $00,$00,$ad,$ae,$b0,$b3,$95,$00
	.byte $00,$bc,$bd,$ac,$b1,$97,$96,$00
	.byte $00,$a0,$be,$bb,$a3,$b2,$98,$00
	.byte $00,$aa,$bf,$b4,$a5,$b7,$99,$00
	.byte $00,$00,$a7,$b5,$b8,$9a,$9b,$00
	.byte $00,$00,$a1,$a2,$b9,$00,$00,$00
	.byte $00,$00,$00,$b6,$00,$af,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$8d,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$94,$00
kbtab_41d_6: ; AltGr 
	.byte $18,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,'@',$00
	.byte $00,$00,$00,$00,$a4,'$',$a3,$00
	.byte $00,$a0,$00,$00,$00,$00,$a4,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$b5,$00,$00,'{','[',$00
	.byte $00,$00,$00,$00,$00,'}',']',$00
	.byte $00,$00,$00,$00,$00,$00,'\',$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$8d,'~',$00,$00,$00,$00
	.byte $00,'|',$00,$00,$00,$00,$94,$00
