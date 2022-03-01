;-------------------------------------------------------------------------------
;Encabezado
;-------------------------------------------------------------------------------
    
; Archivo: Prelab_main.s
; Dispositivo: PIC16F887
; Autor: José Fernando de León González
; Compilador:  pic-as (v2.30), MPLABX v5.40
;
; Programa: Incremento de variable con el timer1
; Hardware: leds y resistencias en PORTA
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

restart_tmr1 macro
 
    MOVLW 0x0B		;Cargando valor inicial de conteo
    MOVWF TMR1H
    
    MOVLW 0x47
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
    
    btfsc TMR1IF  
    call  int_t1
    
pop:				;regresamos los valores de W y STATUS
    SWAPF STATUS_TEMP, W
    MOVWF STATUS
    SWAPF W_TEMP, F
    SWAPF W_TEMP, W
    retfie

;-------------------------------------------------------------------------------
;Subrutinas de interrupción
;-------------------------------------------------------------------------------
    
int_t1:
    restart_tmr1
    INCF seconds
    INCF PORTA
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
    call config_tmr1
    call config_int_enable  ; activamos las interrupciones
    
    banksel PORTA
    			    ;---------------------------------------------------
			    ;TEMPORIZACIÓN TIMER0: 100 ms = 4*4us*(256-60)*32
			    ;---------------------------------------------------
    
;-------------------------------------------------------------------------------
;Loop
;-------------------------------------------------------------------------------

loop:
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
    
    banksel PORTA       ; banco 00
    CLRF PORTA		; limpiamos PORTA
    
    return
    
config_clock:
    banksel OSCCON      ;banco 01
    BSF IRCF2
    BCF IRCF1
    BCF IRCF0           ; IRCF <2:0> -> 100 1 MHz
    
    BSF SCS             ;reloj interno
    
    return

config_tmr1:
    banksel T1CON
    BCF TMR1GE		; Siempre contando
    
    BSF T1CKPS1 
    BCF T1CKPS0		; T1CKPS <1:0> -> 10 Prescaler 1:4 
    
    BCF T1OSCEN		;reloj interno
    BCF TMR1CS
    BSF TMR1ON		;prender TMR1
    
    MOVLW 0x0B		;Cargando valor inicial de conteo
    MOVWF TMR1H
    
    MOVLW 0x47
    MOVWF TMR1L
    
    BCF TMR1IF
   
    return
    
config_int_enable:
    banksel TRISA
    BSF	TMR1IE 
    
    banksel PORTA
    BSF GIE ; INTCON
    BSF PEIE
    
    BCF TMR1IF
    return
   

END