int main () {
  decimal(1,6) dec1;
  decimal(2,8) dec2;
  int d1,d2;
  EXEC SQL select A, B from TAB1 into :d1, :d2 where :d1 > :d2;
  EXEC SQL select A, B from TAB1 into :dec1, :dec2 where :dec1 > :dec2;
  EXEC SQL select A, B from TAB1 into :dec1, :dec2 where :dec1 > :dec2;
  if (x) EXEC SQL select A, B from TAB1 into :dec1, :dec2 where :dec1 > :dec2;
}

