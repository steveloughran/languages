C    My first fortran; newton method of square roots
C    gfortran newton.for
C    ./a.out

      PROGRAM newton77
      REAL root
      REAL square
      INTEGER count
      INTEGER i
      square = 2
      root = 1
      count = 4
C iteration through DO
      DO 10 I = 1, count
      root = (ro ot + square / root) / 2
 10   CONTINUE
      PRINT *, root
      STOP
      END