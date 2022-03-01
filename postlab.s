;-------------------------------------------------------------------------------
;Encabezado
;-------------------------------------------------------------------------------
    
; Archivo: Prelab_main.s
; Dispositivo: PIC16F887
; Autor: José Fernando de León González
; Compilador:  pic-as (v2.30), MPLABX v5.40
;
; Programa: Incremento de variable con el timer1 y led intermitente con timer2, valor de variable segundos en 2 displays
; Hardware: leds y resistencias en PORTA, led en PORTB, displays en PORTC
;
; Creado: 28/02/22
; Última modificación: 28/02/22
    
PROCESSOR 16F887

;-------------------------------------------------------------------------------
;Palabras de configuración 
;-------------------------------------------------------------------------------
    
; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits (INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN)
  CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
  CONFIG  PWRTE = ON            ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
  CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
  CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)
  CONFIG  BOREN = OFF            ; Brown Out Reset Selection bits (BOR enabled)
  CONFIG  IESO = OFF             ; Internal External Switchover bit (Internal/External Switchover mode is enabled)
  CONFIG  FCMEN = OFF            ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is enabled)
  CONFIG  LVP = ON              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
  CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)

;-------------------------------------------------------------------------------
;Librerías incluidas
;-------------------------------------------------------------------------------
  
#include <xc.inc>

;-------------------------------------------------------------------------------
;Macros
;-------------------------------------------------------------------------------

  restart_tmr0 macro
    BANKSEL TMR0        ; banco 00
    MOVLW 0            ; cargar valor inicial a W
    MOVWF TMR0          ; cargar el valor inicial al TIMER0
    BCF T0IF            ; limpiar la bandera  de overflow del TIMER0
    endm
    
  restart_tmr1 macro
 
    MOVLW 0x0D		;Cargando valor inicial de conteo
    MOVWF TMR1H
    
    MOVLW 0x04
    MOVWF TMR1L
    
    BCF TMR1IF
    endm
;-------------------------------------------------------------------------------
;Variables
;-------------------------------------------------------------------------------

  PSECT udata_shr ; Variables en la memoria RAM compartida entre bancos
    
    W_TEMP:	    DS 1	; 1 byte reservado (W Temporal)
    STATUS_TEMP:    DS 1	; 1 byte reservado (STATUS Temporal)  
    seconds:	    DS 1	; 1 byte reservado (aumentar segundos)
    
    flags:		DS 1	; (Variable que indica que display hay que encender en cada instante)
    nibbles:		DS 2	; (Variable que divide los nibbles alto y bajo de valor)
    display_val:	DS 2	; Representación de cada nibble en el display de 7-seg
    
;-------------------------------------------------------------------------------
;Vector Reset
;-------------------------------------------------------------------------------

PSECT VectorReset, class = CODE, abs, delta = 2 ; delta = 2: Las instrucciones necesitan 2 localidades para ejecutarse & abs = absolute: indicamos que contamos a partir de 0x0000
ORG 00h  ; la localidad del vector reset es 0x0000
 
VectorReset:
    PAGESEL main
    GOTO main

;-------------------------------------------------------------------------------
;Vector de interrupción
;-------------------------------------------------------------------------------
ORG 04h	    ; posición 0004h para las interrupciones
push:
    MOVWF W_TEMP		;guardamos los valores previos del STATUS y el W en variables temporales
    SWAPF STATUS, W
    MOVWF STATUS_TEMP
isr:
    
    BTFSC T0IF
    call  int_t0
    btfsc TMR1IF  
    call  int_t1
    btfsc TMR2IF
    call  int_t2
    
pop:				;regresamos los valores de W y STATUS
    SWAPF STATUS_TEMP, W
    MOVWF STATUS
    SWAPF W_TEMP, F
    SWAPF W_TEMP, W
    retfie

;-------------------------------------------------------------------------------
;Subrutinas de interrupción
;-------------------------------------------------------------------------------
int_t0:
    call display_selection
    restart_tmr0
    return
    
display_selection:

    BCF	    PORTB, 1		; Apagamos display de nibble alto
    BCF	    PORTB, 2		; Apagamos display de nibble bajo
    BTFSC   flags, 0		; Verificamos bandera
    goto    display_0		;  
    goto    display_1
    
    return
    
display_0:			
    MOVF    display_val, W	; Movemos display a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTB, 2	; Encendemos display de nibble bajo
    BCF	flags, 0	; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    
    return

display_1:
    MOVF    display_val+1, W	; Movemos display+1 a W
    MOVWF   PORTC		; Movemos Valor de tabla a PORTC
    BSF	PORTB, 1	; Encendemos display de nibble alto
    BSF	flags, 0	; Cambiamos bandera para cambiar el otro display en la siguiente interrupción
    
    return    
    
int_t1:
    restart_tmr1		; reiniciamos el TIMER1
    INCF seconds		; incrementamos la variable de segundos
    INCF PORTA			; corroboramos este incremento con un incremento en PORTA
    return
    
int_t2:
    BCF TMR2IF			; reiniciamos la bandera de interrupción de TIMER2
    BTFSC PORTB, 0
    goto $+3
    BSF PORTB, 0
    goto $+4
    BTFSS PORTB, 0
    goto $+2
    BCF PORTB, 0
    return
    
    
;-------------------------------------------------------------------------------
;Tabla para display de siete segmentos
;-------------------------------------------------------------------------------

PSECT table, class = CODE, abs, delta = 2
ORG 100h 

table:
    CLRF PCLATH
    BSF PCLATH, 0           ; PCLATH en 01
    ANDLW 0X0F
    ADDWF PCL               ; PC = PCLATH + PCL | Sumamos W al PCL para seleccionar un dato en la tabla
    retlw 00111111B         ; 0
    retlw 00000110B         ; 1
    retlw 01011011B         ; 2
    retlw 01001111B         ; 3
    retlw 01100110B         ; 4
    retlw 01101101B         ; 5
    retlw 01111101B         ; 6
    retlw 00000111B         ; 7
    retlw 01111111B         ; 8 
    retlw 01101111B         ; 9
    retlw 01110111B         ; A
    retlw 01111100B         ; b
    retlw 00111001B         ; C
    retlw 01011110B         ; D
    retlw 01111001B         ; C
    retlw 01110001B         ; F

;-------------------------------------------------------------------------------
;main (configuración)
;-------------------------------------------------------------------------------
main:			    
    
    
    call config_ports	    ; configuramos puertos
    call config_clock	    ;configuramos reloj
    call config_tmr0	    ; configuramos TIMER0
    call config_tmr1	    ;configuramos TIMER1
    call config_tmr2	    ;configuramos TIMER2
    call config_int_enable  ; activamos las interrupciones
    
    banksel PORTA
    			    ;---------------------------------------------------
			    ;TEMPORIZACIÓN TIMER0:  2 ms
			    ;TEMPORIZACIÓN TIMER1:  1000 ms
			    ;TEMPORIZACIÓN TIMER2:  500 ms
			    ;---------------------------------------------------
    
;-------------------------------------------------------------------------------
;Loop
;-------------------------------------------------------------------------------

loop:
    call    nibble_save		; Guardamos nibble alto y bajo de valor
    call    display		; Guardamos los valores a enviar en PORTC para mostrar valor en hex
    goto loop
;-------------------------------------------------------------------------------
;subrutinas
;-------------------------------------------------------------------------------
config_ports:
    
    banksel ANSEL       ; banco 11
    CLRF ANSEL		; pines digitales
    CLRF ANSELH
    
    banksel TRISA       ; banco 01
    CLRF TRISA		; PORTA como salida
    CLRF TRISB		; PORTB como salida
    CLRF TRISC		; PORTC como salida
    
    banksel PORTA       ; banco 00
    CLRF PORTA		; limpiamos PORTA
    CLRF PORTB		; limpiamos PORTB
    CLRF PORTC		; limpiamos PORTC
    
    return
    
config_clock:
    banksel OSCCON      ;banco 01
    BCF IRCF2
    BSF IRCF1
    BSF IRCF0           ; IRCF <2:0> -> 011 500 kHz
    
    BSF SCS             ;reloj interno
    
    return

config_tmr0:
    banksel TRISA	; banco 01
    BCF T0CS            ; TIMER0 como temporizador
    BCF PSA             ; Prescaler a TIMER0
    BCF PS2
    BCF PS1
    BCF PS0             ; PS<2:0> -> preescaler 000 1:1
    
    restart_tmr0    
    return
    
config_tmr1:
    banksel T1CON
    BCF TMR1GE		; Siempre contando
    
    BCF T1CKPS1 
    BSF T1CKPS0		; T1CKPS <1:0> -> 01 Prescaler 1:2 
    
    BCF T1OSCEN		;reloj interno
    BCF TMR1CS
    BSF TMR1ON		;prender TMR1
    
    MOVLW 0x0D		;Cargando valor inicial de conteo
    MOVWF TMR1H
    
    MOVLW 0x04
    MOVWF TMR1L
    
    BCF TMR1IF
   
    return
    
config_tmr2:
    banksel PR2
    MOVLW   244		    ; Valor para interrupciones cada 50ms
    MOVWF   PR2		    ; Cargamos litaral a PR2
    
    BANKSEL T2CON	    ; Cambiamos a banco 00
    BSF	    T2CKPS1	    ; Prescaler 1:16
    BSF	    T2CKPS0
    
    BSF	    TOUTPS3	    ;Postscaler 1:16
    BSF	    TOUTPS2
    BSF	    TOUTPS1
    BSF	    TOUTPS0
    
    BSF	    TMR2ON	    ; Encendemos TMR2
    return
    
config_int_enable:
    banksel TRISA
    BSF	TMR1IE		    ; Habilitar interrupción del TIMER1 
    BSF TMR2IE		    ; Habilitar interrupción del TIMER2
    
    banksel PORTA
    BSF GIE		    ; INTCON: Habilitar interrupciones globales	    
    BSF PEIE		    ; Habilitar interrupciones de periféricos
    
    BCF TMR1IF		    ; Limpiar bandera de interrupción de TIMER1
    BCF TMR2IF		    ; Limpiar bandera de interrupción de TIMER2
    
    BSF T0IE		    ; Habilitar interrupción del TIMER0
    BCF T0IF		    ; Limpiar bandera de interrupciones
    return
    
nibble_save:
    MOVLW   0x0F		    
    ANDWF   seconds, W		; Se hace un AND de value con el valor en W para guardar solo el nibble bajo
    MOVWF   nibbles		; Guardar el nibble bajo en el primer registro de la variable nibbles	
				 
    MOVLW   0xF0		      
    ANDWF   seconds, W		; Se hace un AND con el valor en W para guardazr solo el nibble alto	  
    MOVWF   nibbles+1		; Enviar el valor al segundo registro de la variable nibbles	      
    SWAPF   nibbles+1, F	; Utilizar un SWAPF para mover los nibbles de su posición high a la posición low	      
    
    return
    
display:
    MOVF    nibbles, W		; Movemos nibble bajo a W
    call    table		; Buscamos valor a cargar en PORTC
    MOVWF   display_val		; Guardamos en el primer registro de la variable display_val
    
    MOVF    nibbles+1, W	; Movemos nibble alto a W
    call    table		; Buscamos valor a cargar en PORTC
    MOVWF   display_val+1	; Guardamos en en el segundo registro de la variable display_val
    
    return     
END


