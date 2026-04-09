
#include <iomanip>
#include <iostream>
#include <random>
#include <vector>

class matrix {
public: 
  std::vector<double>              xs;
  std::vector<std::vector<double>> as;
  std::vector<double>              bs;
  int size;

  matrix (std::vector<std::vector<double>> &m) {
    int size = m.size() - 1;
    this->xs = {};
    this->as = {};
    this->bs = {};

    for (int i = 0; i < size; i++) {
      std::vector<double> a_iline;
      for (int j = 0; j < size; j++) {
        a_iline.push_back(m[i][j]);
      }
      this->as.push_back(a_iline);
    }

    for (int i = 0; i < size; i++) {
      this->bs.push_back(m[i][size+1]);
    }

    std::vector<double> xs;
    for (int i = 0; i < size; i++) {
      this->xs.push_back(0.0);
    }
  }

  matrix(int n) {
    std::vector<std::vector<double>> m(n, std::vector<double> (n + 1, 0.0));

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(-100000.0, 10000.0);

    double margem = 1e-15; 

    for (int i = 0; i < n; ++i) {
      double soma_linha = 0.0;

      for (int j = 0; j < n + 1; ++j) {
        m[i][j] = dis(gen);

        if (i != j && j < n) {
          soma_linha += std::abs(m[i][j]);
        }
      }

      double valor_dominante = soma_linha + margem;
      m[i][i] = std::copysign(valor_dominante, m[i][i]); 
    }

    auto m_ = matrix(m);
    this->xs = m_.xs;
    this->as = m_.as;
    this->bs = m_.bs;
    this->size = m_.size;
  }

  void print() {
    for (int i = 0; i < this->as.size(); ++i) {
      for (int j = 0; j < this->as[i].size(); ++j) {
        std::cout << std::setw(10) << std::fixed << std::setprecision(4) << this->as[i][j] << " ";
      }

      if (i < this->bs.size()) {
        std::cout << "| " << std::setw(10) << std::fixed << std::setprecision(4) << this->bs[i];
      }
      std::cout << "\n";
    }
  }

  inline void jacobi_step ( 
    const int begin, const int end, 
    const std::vector<double> &old_xs
  ) {

    for(int i = begin; i < end; i++) {
      double aii = this->as[i][i];
      double sum = this->bs[i];
      for (int j = 0; j < old_xs.size(); j++) {
        if (i!=j) {
          sum -= this->as[i][j] * old_xs[j];
        }
      }

      this->xs[i] = sum/aii;
    }
  }

  inline bool converged
  (const std::vector<double> &old_xs, 
   const double ε) {
    double highest_diff = 0.0;

    for (int i = 0; i < this->xs.size(); i++) {
      double current_diff = std::abs(this->xs[i] - old_xs[i]);
      if (current_diff > highest_diff) {
        highest_diff = current_diff;
      }
    }

    return highest_diff < ε;
  }

};


