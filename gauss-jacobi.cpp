
#include <iostream>
#include <vector>

struct matrix {
  std::vector<double> xs;
  std::vector<std::vector<double>> as;
  std::vector<double> bs;
  int size;
};

void jacobi_serial(
  matrix &m, std::vector<double> &old_xs
) {
  int size = m.size;

  old_xs = m.xs; // memory leaks?
  auto &as = m.as;
  auto &bs = m.bs;

  for(int i = 0; i < size; i++) {
    double aii = as[i][i];
    double sum = bs[i];
    for (int j = 0; j < size; j++) {
      if (i!=j) {
        sum -= as[i][j] * old_xs[j];
      }
    }

    m.xs[i] = sum/aii;
  }
}

int main() {
    int tamanho_sistema = 2500; // Bora testar com uma matriz 1000x1000
    
    double sum = 0;
    int times  = 10;
    for(int i = 0; i < times; i++) {
      std::cout << "Gerando matriz...\n";
      std::vector<std::vector<double>> matriz_bruta = gerar_matriz_dominante(tamanho_sistema);
      
      std::cout << "Estruturando matriz no formato da struct...\n";
      matrix m = mkmatrix(matriz_bruta);
      
      std::cout << "Resolvendo sistema...\n";
      // Chama a função que roda o laço e cronometra
      sum += resolver_ate_convergir_(m, 1e-15); 
    }
    
    std::cout << "average: " << sum/times;
    return 0;
}
