// вложенный однострочный цикл в полный цикл
program t6;
var I, J, K: Integer;
label wh1;
label wh2;
begin
  I := 0;
  J := 100;
  K := 0;

  wh1: if I <= J  then begin
      WriteLn('I / J = ', I, ' / ', J);

    wh2: if J > K  then begin
      J -= 1;
goto wh2; end;

  goto wh1; end;

  WriteLn('I / J = ', I, ' / ', J);

end.
