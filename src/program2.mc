int y;
int x;
int[10] A;
y := x;
while (y > 0) {
    A[y]:= y*2;
    y:= y - 1;
}
write A;
