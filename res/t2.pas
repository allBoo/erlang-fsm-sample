// многострочный цикл с блоком внутри
var I, Z, X: Integer;

begin
  I := 10;
  Z := 10;
  X := 10;

  while I > 0 do begin
    if Odd(I) then begin
      Z:=Z * X;
    end;
    I:=I div 2;
    X:=Sqr(X);
  end;

  WriteLn(I);
  WriteLn(Z);
  WriteLn(X);
end.
