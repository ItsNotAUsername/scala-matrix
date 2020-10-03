object ScalarOps:
  
  extension [T: Numeric](x: T)
    def * (matrix: Matrix[T]): Matrix[T] = matrix * x