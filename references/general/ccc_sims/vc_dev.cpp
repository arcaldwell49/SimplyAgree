// C++ version of .vc_dev() in fast_vc.R (REML criterion of the
// variance-components model from subject sufficient statistics).
// st columns: nx, ny, Sx, Sy, Q. Returns Inf for an unusable point.
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double vc_dev_cpp(NumericVector th, NumericMatrix st, NumericVector w) {
  const double t1 = th[0], t2 = th[1];
  const int g = st.nrow();
  double X11 = 0, X12 = 0, X22 = 0, Xy1 = 0, Xy2 = 0, yty = 0, logdet = 0,
    N = 0;
  for (int i = 0; i < g; ++i) {
    const double wi = w[i];
    if (wi <= 0) continue;
    const double nx = st(i, 0), ny = st(i, 1), Sx = st(i, 2), Sy = st(i, 3),
      Q = st(i, 4);
    const double n = nx + ny, S = Sx + Sy;
    const double M11 = 1 + t1 * t1 * n, M12 = t1 * t2 * nx,
      M13 = t1 * t2 * ny, M22 = 1 + t2 * t2 * nx, M33 = 1 + t2 * t2 * ny;
    const double det = M11 * M22 * M33 - M12 * M12 * M33 - M13 * M13 * M22;
    const double i11 = M22 * M33 / det, i12 = -M12 * M33 / det,
      i13 = -M13 * M22 / det, i22 = (M11 * M33 - M13 * M13) / det,
      i23 = M12 * M13 / det, i33 = (M11 * M22 - M12 * M12) / det;
    const double a[3] = {t1 * n, t2 * nx, t2 * ny};
    const double b[3] = {t1 * ny, 0, t2 * ny};
    const double c[3] = {t1 * S, t2 * Sx, t2 * Sy};
    auto q = [&](const double* u, const double* v) {
      return u[0] * (i11 * v[0] + i12 * v[1] + i13 * v[2]) +
        u[1] * (i12 * v[0] + i22 * v[1] + i23 * v[2]) +
        u[2] * (i13 * v[0] + i23 * v[1] + i33 * v[2]);
    };
    X11 += wi * (n - q(a, a));
    X12 += wi * (ny - q(a, b));
    X22 += wi * (ny - q(b, b));
    Xy1 += wi * (S - q(a, c));
    Xy2 += wi * (Sy - q(b, c));
    yty += wi * (Q - q(c, c));
    logdet += wi * std::log(det);
    N += wi * n;
  }
  const double dX = X11 * X22 - X12 * X12;
  if (!(X11 > 0) || !(dX > 0)) return R_PosInf;
  const double b1 = (X22 * Xy1 - X12 * Xy2) / dX;
  const double b2 = (X11 * Xy2 - X12 * Xy1) / dX;
  const double rss = yty - b1 * Xy1 - b2 * Xy2;
  if (!(rss > 0)) return R_PosInf;
  const double df = N - 2;
  return logdet + std::log(dX) + df * (1 + std::log(2 * M_PI * rss / df));
}
