C    Marker
      PROGRAM newton77
      REAL root
      REAL square
      INTEGER count
      INTEGER i
      square = 2
      root = 1
      count = 4
C iteration through DO
      DO 10 I = 1, 4
      root = (root + square / root) / 2
 10   CONTINUE
      PRINT *, root
      STOP
      END