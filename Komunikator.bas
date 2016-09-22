'obliczenia parametrów konfiguracyjnych
Const Prescfc = 0                                           'potêga dzielnika czêstotliwoœci taktowania procesora
Const Fcrystal =(14745600 /(2 ^ Prescfc))                   'czêstotliwoœæ po przeskalowaniu
'sta³e konfiguracujne USARTów
Const Baundrs0 = 115200                                     'prêdkoœæ transmisji po RS [bps] USART0
Const _ubrr0 =(((fcrystal / Baundrs0) / 16) - 1)            'potrzebne w nastêpnych zadaniach
Const Baundrs1 = Baundrs0                                   'prêdkoœæ transmisji po RS [bps] USART1
Const _ubrr1 =(((fcrystal / Baundrs1) / 16) - 1)            'potrzebne w nastêpnych zadaniach

Const Br = 115200 / 2
Const Rfm_baud = 10000 / 29 / 1 / Br - 1




'konfigurowanie mikrokontrolera
$regfile = "m644pdef.dat"                                   'plik konfiguracyjny z literk¹ "p" w nazwie
$crystal = Fcrystal
'$baud = Baundrs0    'zbêdne gdy inicjalizacja w zdefiniowanej procedurze

'aliasy rejestrów procesora
Temp Alias R16
Temph Alias R17
Rstemp Alias R18
Rsdata Alias R19
Bajth Alias R20
Bajtl Alias R21

Dim Bajt As Byte
Dim Bufor(64) As Byte
Bufor(1) = &HAA
Bufor(2) = &HAA
Bufor(3) = &H2D
Bufor(4) = &HD4
'Bufor(5) = &H03
Bufor(6) = &HAA

Dim B_count As Byte
B_count = 0
Const B_max = 5
Ss_pin Alias 4
Ss Alias Portb.ss_pin

Led_pin Alias 5
Led Alias Portd.led_pin                                     'definicja portow dla diody kontrolnej
SBI Ddrd,led_pin

On Urxc Usart0_rx Nosave
'On Urxc1 Usart1_rx Nosave                                   'deklaracja przerwania URXC1 (odbiór znaku USART1)
'On Utxc1 Usart1_tx_end Nosave                               'deklaracja przerwania UTXC1, koniec nadawania





'zmniejszenie czêstotliwoœci taktowania procesora
ldi temp,128
!Out clkpr,temp                                             'ustawienie bitu 7, CLKPR = 128
ldi temp,prescfc                                            'aktualizacja CLKPR dopiero po uprzednim ustawienu bitu 7
!Out clkpr,temp                                             'CLKPR = Prescfc

rcall usart_init                                            'inicjalizacja USARTów i w³¹czenie przerwañ


sbi ddrB, 2                                                 'pin jako wejscie
SBI portb,2                                                 'pullup
cbi ddrb,2
Config Int2 = Falling
On Int2 Int2_radio Nosave                                   'inicjalizacja przerwania int2
Enable Int2

Const _presctimer = 256
Const _fpr = 5                                              'czêstotliwoœc próbkowania
Const _ocrtimer = Fcrystal / _presctimer / _fpr -1


Config Timer1 = Timer , Prescale = _presctimer , Compare A = Disconnect , Clear Timer = 1
Stop Timer1
Timer1 = 0
Ocr1a = _ocrtimer

On Oc1a Czasomierz Nosave
'Enable Oc1a

                                               'wlaczenie przerwania
Const Adres = &HD4

rcall rfm_spi_init
rcall rfm12_init

rcall ustaw_moj_adres
rcall wlacz_odbiornik
'Start Timer1
sei

Do

Loop


'''''''''''''''''''''''''''''''''''''''''''''''''''''''PRZERWANIA'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 Usart0_rx:
 push rstemp                                                'o ile potrzeba - sprawdziæ
 Push rsdata
 in rstemp,sreg                                             'o ile potrzeba  - sprawdziæ
 push rstemp                                                'o ile potrzeba - sprawdziæ

 'Print "przerwanie"
   rcall wyslij_znak

 pop rstemp
 !out sreg,rstemp
 pop rsdata
 pop rstemp
 Return
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Usart1_rx:

 push rstemp                                                'o ile potrzeba - sprawdziæ
 Push rsdata
 in rstemp,sreg                                             'o ile potrzeba  - sprawdziæ
 push rstemp                                                'o ile potrzeba - sprawdziæ

 pop rstemp
 !out sreg,rstemp
 pop rsdata
 pop rstemp
Return
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Usart1_tx_end:                                              'przerwanie konca wysylania danych przez master ma jedynie zamienic stan na lini Te
    'Te = 0                                                  'przejscie w tryb odbioru
Return
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Int2_radio:
 push rstemp                                                'o ile potrzeba - sprawdziæ
 Push rsdata
 push bajth
 push bajtl
 in rstemp,sreg                                             'o ile potrzeba  - sprawdziæ
 push rstemp
  'Print "!"                                                 'o ile potrzeba - sprawdziæ
 rcall sprawdz_status

   sbrc bajth,7
   rcall wyslij_pakiet

 pop rstemp
 !out sreg,rstemp
 pop bajtl
 pop bajth
 pop rsdata
 pop rstemp

Return

Czasomierz:
rcall sprawdz_status
Enable Int2
rcall dioda
rcall wlacz_nadajnik
 ldi bajtl,&haa
 rcall wyslij_bajt
Return

'''''''''''''''''''''''''''''''''''''''''''''''''''''''PROCEDURY'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
!usart_init:
'procedura inicjalizacji USARTów
   ldi temp,0
   !out ubrr0h,temp                                         'bardziej znacz¹cy bajt UBRR USART0
   '!out ubrr1h,temp
   ldi temp,_ubrr0
   !out ubrr0l,temp                                         'mniej znacz¹cy bajt UBRR USART0
   ldi temp,_ubrr1
   '!out ubrr1l,temp                                         'mniej znacz¹cy bajt UBRR USART1
   ldi temp,24                                              'w³¹czone odbiorniki i nadajniki USARTów
   !out ucsr0b,temp
   '!out ucsr1b,temp
   ldi temp,6                                               'N8bit
   !out ucsr0C,temp
   '!out ucsr1C,temp
   'ustawienia RS485
  ' Te = 0                                                   'domyœlnie stan odbioru
   'sbi ddrd,Te_pin                                          'wyjœcie TE silnopr¹dowe
   'w³¹czenie przerwañ
   Enable Urxc                                              'Niepotrzebne Przerwanie Od Komputera
   'Enable Urxc
   'Enable Urxc1
   'Enable Utxc1
ret
  ''''''''''''''''''''''''''''''''''''''''''
!rfm_spi_init:
sbi ddrb,7
Cbi ddrb,6
sbi ddrb,5
sbi ddrb,4

SBI portb,7
SBI portb,6
SBI portb,5
SBI portb,4
Config Spi = Hard , Interrupt = Off , Data Order = Msb , Master = Yes , Polarity = Low , Phase = 0 , Clockrate = 16 , Noss = 1
Spiinit
  ''''''''''''''''''''''''''''''''''''''''''
ret
!rfm12_init:
Print "rfm_init"
   rcall soft_reset
   Waitms 250
                                               'configuration setting command
   ldi bajth, &h80
   ldi bajtl, &hd7
   rcall wyslij_spi

                                               'power management command
   ldi bajth,&h82
   ldi bajtl,&h09
   rcall wyslij_spi

                                               'frequency setting command
   ldi bajth,&ha6
   ldi bajtl,&h40
   rcall wyslij_spi

                                               'data rate command
   ldi bajth,&hc6
   ldi bajtl,rfm_baud
   rcall wyslij_spi

                                               'receiver control command
   ldi bajth,&h94
   ldi bajtl,&ha0
   rcall wyslij_spi

                                               'data filter clock recovery command
   ldi bajth,&hc2
   ldi bajtl,&hac
   rcall wyslij_spi

                                               'FIFO & reset command
   ldi bajth,&hca
   ldi bajtl,&h81
   rcall wyslij_spi

                                               'sync pattern command
   ldi bajth,&hce
   ldi bajtl,&hd4
   rcall wyslij_spi

                                               'aut freq ctrl command
   ldi bajth,&hc4
   ldi bajtl,&h83
   rcall wyslij_spi

                                               'TX control command
   ldi bajth,&h98
   ldi bajtl,&h50
   rcall wyslij_spi

                                               'PLL control command
   ldi bajth,&hcc
   ldi bajtl,&h17
   rcall wyslij_spi

                                               'Wake Up Control Command
   ldi bajth,&he0
   ldi bajtl,&h00
   rcall wyslij_spi

                                               'low duty command
   ldi bajth,&hc8
   ldi bajtl,&h00
   rcall wyslij_spi

                                               'low battery command
   ldi bajth,&hc0
   ldi bajtl,&hc0
   rcall wyslij_spi

ret
''''''''''''''''''''''''''''''''''''''''''
!dioda:                                                     'zmiana stanu diody
   Sbis portd,led_pin
      rjmp zapal_led
   sbic portd,led_pin
      rjmp zgas_led
   !zapal_led:
      Led = 1
      rjmp wyslij
   !zgas_led:
      Led = 0
      !wyslij:
      ret
 ''''''''''''''''''''''''''''''''''''''''''
 !wyslij_spi:

   Ss = 0

   !out spdr,bajth
   rcall czekaj_spdr
   in bajth,spdr

   !out spdr,bajtl
   rcall czekaj_spdr
   in bajtl,spdr
   Ss = 1

 ret
 ''''''''''''''''''''''''''''''''''''''''''
 !czekaj_spdr:
 sbis spsr,7
 rjmp czekaj_spdr
 ret
 ''''''''''''''''''''''''''''''''''''''''''
  !sprawdz_status:
     ldi bajth,0
     ldi bajtl,0
     rcall wyslij_spi
  ret
  ''''''''''''''''''''''''''''''''''''''''''
!soft_reset:
   ldi bajth,&hfe
   ldi bajtl,0
   rcall wyslij_spi
ret
  ''''''''''''''''''''''''''''''''''''''''''
  !odczytaj_FIFO:
  ldi bajth,&hb0
  ldi bajtl,0
  rcall wyslij_spi
    ' sts {bajt},bajtl
   'Print Bajt
  rcall reset_fifo

  ret
  ''''''''''''''''''''''''''''''''''''''''''
   !wyslij_bajt:
   ldi Bajth, &HB8
   rcall wyslij_spi
   ret
  ''''''''''''''''''''''''''''''''''''''''''
  !ustaw_moj_adres:
   ldi bajtH,&hce
   ldi bajtL,adres
   rcall wyslij_spi
  ret
  ''''''''''''''''''''''''''''''''''''''''''
!reset_fifo:
  ldi bajth,&hca
  ldi bajtl,&h81
  rcall wyslij_spi

  ldi bajth,&hca
  ldi bajtl,&h83
  rcall wyslij_spi
ret
  ''''''''''''''''''''''''''''''''''''''''''
!wlacz_nadajnik:
  ldi bajth,&h82
  ldi bajtl,&h39
  rcall wyslij_spi
  ret
  ''''''''''''''''''''''''''''''''''''''''''
!wlacz_odbiornik:
  ldi bajth,&h82
  ldi bajtl,&hc9
  rcall wyslij_spi

ret
  ''''''''''''''''''''''''''''''''''''''''''
!wylacz_transeiver:
  ldi bajth,&h82
  ldi bajtl,&h01
  rcall wyslij_spi
ret
  ''''''''''''''''''''''''''''''''''''''''''
  !wyslij_pakiet:
  lds temp,{b_count}
  cpi temp,b_max
  sbis sreg,2
   rjmp koniec_transmisji
  inc temp
  sts {b_count},temp
  'Print B_count
  Loadadr Bufor(1) , X
  dec temp
   add xl,temp
   ldi temp,0
   adc xh,temp
   ld bajtl,x
   rcall wyslij_bajt

  ret
  !koniec_transmisji:

  Loadadr Bufor(5) , X
  ld rstemp,X
  !out udr0,rstemp
    B_count = 0
  'Disable Int2
  rcall wlacz_odbiornik

  ret

  !wyslij_znak:
  in rsdata, udr0
  Loadadr Bufor(5) , X
  st X, rsdata
  rcall wlacz_nadajnik

  ldi bajtl,&Haa
  rcall wyslij_bajt

  ret