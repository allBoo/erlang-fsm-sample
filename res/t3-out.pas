// вложенные многострочные циклы
program t3;

var I, J, K: Integer;

label wh1;
label wh2;
begin
  I := 0;
  K := 10;

  wh1: if I < K  then begin
      J := 0;
    wh2: if J < K  then begin
          J += 1;
      WriteLn('I / J = ', I, ' / ', J);
    goto wh2; end;
    I += 1;
  goto wh1; end;

end.

