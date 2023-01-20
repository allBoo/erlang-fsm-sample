// несколько циклов
program t5;
var I, J, K: Integer;
label wh1;
label wh2;
label wh3;
label wh4;
begin
  I := 0;
  J := 100;
  K := 0;

  wh1: if I <= J  then begin
    wh2: if J > K  then begin
      J -= 1;
goto wh2; end;
goto wh1; end;

  WriteLn('I / J = ', I, ' / ', J);

  I := 0;
  J := 100;
  K := 0;

  wh3: if I <= J  then begin
    wh4: if J > K  then begin
      J -= 1;
goto wh4; end;
goto wh3; end;

  WriteLn('I / J = ', I, ' / ', J);

end.
