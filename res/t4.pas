// вложенные однострочные циклы
program t4;
var I, J, K: Integer;
begin
  I := 0;
  J := 100;
  K := 0;

  while I <= J do
    while J > K do
      J -= 1;

  WriteLn('I / J = ', I, ' / ', J);

end.
