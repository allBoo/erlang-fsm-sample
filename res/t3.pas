// вложенные многострочные циклы
program t3;

var I, J, K: Integer;

begin
  I := 0;
  K := 10;

  while I < K do
  begin
    J := 0;
    while J < K do
    begin
      J += 1;
      WriteLn('I / J = ', I, ' / ', J);
    end;
    I += 1;
  end;

end.

