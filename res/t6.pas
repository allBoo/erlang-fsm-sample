// вложенный однострочный цикл в полный цикл
program t6;
var I, J, K: Integer;
begin
  I := 0;
  J := 100;
  K := 0;

  while I <= J do
  begin
    WriteLn('I / J = ', I, ' / ', J);

    while J > K do
      J -= 1;
  end;

  WriteLn('I / J = ', I, ' / ', J);

end.
