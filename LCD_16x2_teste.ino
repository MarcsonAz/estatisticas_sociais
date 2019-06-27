#include <LiquidCrystal.h>


int segundo = 0x00,
    minuto = 30,
    hora = 15;
const int rs = 12, en = 11, d4 = 5, d5 = 4, d6 = 3, d7 = 2;
LiquidCrystal lcd(rs, en, d4, d5, d6, d7);

void setup() {
  // set up the LCD's number of columns and rows:
  lcd.begin(16, 2);
  lcd.setCursor(2,1);
  lcd.print("Smart");
  lcd.print(" ");
  lcd.print("Eagle");
}

void loop() {
  lcd.setCursor(3,0);
  segundo = segundo + 1;
  if(segundo == 60){
    segundo = 0;
    minuto = minuto + 1;
    if(minuto == 60){
      minuto = 0;
      hora = hora+1;
      if(hora == 25){
        hora = 0;
      }
    }
  }
  lcd.print(hora);
  lcd.print(":");
  lcd.print(minuto);
  lcd.print(":");
  lcd.print(segundo);
  delay(1000);
}
