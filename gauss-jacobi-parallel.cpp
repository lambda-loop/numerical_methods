
#include <iostream>
#include <thread>
#include <vector>
#include <chrono>
#include "matrix.h"

#define SIZE        10
// #define TIMES      100
#define MAX_ITERS 1000

auto m = matrix(SIZE);

inline int serial () {
  bool converged = false;
  int iters = 0;

  std::vector<double> old_xs = m.xs;

  auto time0 = std::chrono::high_resolution_clock::now();
  while (!converged&& iters < MAX_ITERS) {
    old_xs = m.xs;
    m.jacobi_step(0, SIZE, old_xs);
    converged = m.converged(old_xs, 1e-15);
    iters++;
  }

  auto time1 = std::chrono::high_resolution_clock::now();

  std::chrono::duration<double> duracao = time1 - time0;

  std::cout << "--- Relatório de Execução ---\n";
  if (converged) {
    std::cout << "Status: Convergiu com sucesso!\n";
  } else {
    std::cout << "Status: ALERTA! Atingiu o limite de " << MAX_ITERS << " iterações sem convergir.\n";
  }
  std::cout << "Iterações: " << iters << "\n";
  std::cout << "Tempo gasto: " << duracao.count() << " segundos.\n";
  std::cout << "-----------------------------\n";
  return duracao.count();
}


int main () {
  serial();
  return 0;
}


