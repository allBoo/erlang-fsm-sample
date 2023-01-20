// многострочный цикл с блоком внутри
var I, Z, X: Integer;

label wh1;
begin
  I := 10;
  Z := 10;
  X := 10;

  wh1: if I > 0  then begin
    if Odd(I) then begin
      Z:=Z * X;
    end;
    I:=I div 2;
    X:=Sqr(X);
  goto wh1; end;

  WriteLn(I);
  WriteLn(Z);
  WriteLn(X);
end.
