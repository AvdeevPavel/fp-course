program HelloWorld;

function add (x:Integer; y : Integer) : Boolean;
var
result:Integer;
begin
x := x + y;
end;

function emptyF ():Integer;
begin
end;

procedure print (str:String);
var
index:Integer ;
mask : Boolean ;
ar : array [1 .. 20] of Integer;
begin
index:=0;
mask :=True   ;
println (str, mask);
mask:= False;
end;

procedure emptyP ();
begin
end;

function main () : Integer;
begin
print("lolo");
emptyF();
end;




