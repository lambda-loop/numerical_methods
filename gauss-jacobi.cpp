
#include <vector>

struct matrix {
  std::vector<double> xs;
  std::vector<std::vector<double>> as;
  std::vector<double> bs;
  int size;
};

void serial(matrix &m) {
  int size = m.size;
  std::vector<double> xs(m.xs);

  auto &as = m.as;
  auto &bs = m.bs;

  for(int i = 0; i < size; i++) {
    double aii = as[i][i];
    double sum = bs[i];
    for (int j = 0; j < size; j++) {
      if(i!=j) {
        sum -= as[i][j] * xs[j];
      }
    }
    xs[i] = sum/aii;
  }
  m.xs = std::move(xs);
}

