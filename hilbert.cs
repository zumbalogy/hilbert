// http://stackoverflow.com/questions/499166/mapping-n-dimensional-value-to-a-point-on-hilbert-curve

namespace HilbertExtensions {
  using System;
  // Convert between Hilbert index and N-dimensional points.
  //
  // The Hilbert index is expressed as an array of transposed bits.
  //
  // Example: 5 bits for each of n = 3 coordinates.
  // 15-bit Hilbert integer = A B C D E F G H I J K L M N O is stored as its Transpose
  //
  //                                         ^   7
  // X[0] = A D G J M                    X[2]|  /
  // X[1] = B E H K N        <------->       | /X[1]
  // X[2] = C F I L O           axes         |/
  //        high low                         0------> X[0]
  //
  // Note: This algorithm is derived from work done by John Skilling and published in "Programming the Hilbert curve".

  public static class HilbertCurveTransform {
    // Convert the Hilbert index into an N-dimensional point expressed as a vector of uints.
    //
    // Note: In Skilling's paper, this function is named TransposetoAxes.
    //
    // param transposedIndex: The Hilbert index stored in transposed form.
    // param bits: Number of bits per coordinate.
    // returns: Coordinate vector.
    public static uint[] HilbertAxes(this uint[] transposedIndex, int bits) {
      var X = (uint[])transposedIndex.Clone();
      int n = X.Length; // n: Number of dimensions
      uint N = 2U << (bits - 1), P, Q, t;
      int i;
      // Gray decode by H ^ (H/2)
      t = X[n - 1] >> 1;
      // Corrected error in Skilling's paper on the following line. The appendix had i >= 0 leading to negative array index.
      for (i = n - 1; i > 0; i--) {
        X[i] ^= X[i - 1];
      }
      X[0] ^= t;
      // Undo excess work
      for (Q = 2; Q != N; Q <<= 1) {
        P = Q - 1;
        for (i = n - 1; i >= 0; i--) {
          if ((X[i] & Q) != 0U) {
            X[0] ^= P; // invert
          } else {
            t = (X[0] ^ X[i]) & P;
            X[0] ^= t;
            X[i] ^= t;
          }
        }
      } // exchange
      return X;
    }

    // Given the axes (coordinates) of a point in N-Dimensional space, find the distance to that point along the Hilbert curve.
    // That distance will be transposed; broken into pieces and distributed into an array.
    //
    // The number of dimensions is the length of the hilbertAxes array.
    //
    // Note: In Skilling's paper, this function is called AxestoTranspose.
    //
    // param hilbertAxes; Point in N-space.
    // param bits; Depth of the Hilbert curve. If bits is one, this is the top-level Hilbert curve.
    // returns: The Hilbert distance (or index) as a transposed Hilbert index.
    public static uint[] HilbertIndexTransposed(this uint[] hilbertAxes, int bits) {
      var X = (uint[])hilbertAxes.Clone();
      var n = hilbertAxes.Length; // n: Number of dimensions
      uint M = 1U << (bits - 1), P, Q, t;
      int i;
      // Inverse undo
      for (Q = M; Q > 1; Q >>= 1) {
        P = Q - 1;
        for (i = 0; i < n; i++) {
          if ((X[i] & Q) != 0) {
            X[0] ^= P; // invert
          } else {
            t = (X[0] ^ X[i]) & P;
            X[0] ^= t;
            X[i] ^= t;
          }
        }
      } // exchange
      // Gray encode
      for (i = 1; i < n; i++) {
        X[i] ^= X[i - 1];
      }
      t = 0;
      for (Q = M; Q > 1; Q >>= 1) {
        if ((X[n - 1] & Q) != 0) {
          t ^= Q - 1;
        }
      }
      for (i = 0; i < n; i++) {
        X[i] ^= t;
      }
      return X;
    }

    static public void Main() {
      uint[] foo = HilbertIndexTransposed(new uint [] { 100, 5, 90 }, 32);
      Console.WriteLine("hilbert axes");
      Console.WriteLine(foo[0]);
      Console.WriteLine(foo[1]);
      Console.WriteLine(foo[2]);
      Console.WriteLine("len");
      Console.WriteLine(foo.GetLength(0));
      Console.WriteLine("-");
      Console.WriteLine("transpose");
      uint[] bar = HilbertAxes(foo, 32);
      Console.WriteLine(bar[0]);
      Console.WriteLine(bar[1]);
      Console.WriteLine(bar[2]);
      Console.WriteLine("len");
      Console.WriteLine(bar.GetLength(0));
      Console.WriteLine("-");
    }
  }
}
