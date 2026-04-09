
#include <iostream>
#include <thread>
#include <vector>
#include <chrono>
#include "matrix.h"

#define SIZE        20000
// #define TIMES      100
#define MAX_ITERS 1000

matrix m = matrix(SIZE);
const int SIZEB = (SIZE-1);

inline int serial () {
  bool converged = false;
  int iters = 0;

  std::vector<double> old_xs = m.xs;

  auto time0 = std::chrono::high_resolution_clock::now();
  while (!converged&& iters < MAX_ITERS) {
    old_xs = m.xs;

    m.jacobi_step(0, SIZEB, old_xs);
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

inline int parallel2 () {
  bool converged = false;
  int iters = 0;

  std::vector<double> old_xs = m.xs;

  auto time0 = std::chrono::high_resolution_clock::now();
  while (!converged&& iters < MAX_ITERS) {
    old_xs = m.xs;

    std::thread t1([&]() {
      m.jacobi_step(0, SIZEB/2, old_xs);
    });

    std::thread t2([&]() {
      m.jacobi_step(SIZEB/2, SIZEB, old_xs);
    });

    t1.join(); t2.join();
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

inline int parallel3();
inline int parallel8();

int main () {
  // matrix m = matrix(SIZE);
  // m.print();
  // serial();
  // parallel2();
  parallel8();
  return 0;
}



inline int parallel8 () {
  bool converged = false;
  int iters = 0;

  std::vector<double> old_xs = m.xs;

  auto time0 = std::chrono::high_resolution_clock::now();
  while (!converged&& iters < MAX_ITERS) {
    old_xs = m.xs;

    std::thread t0([&]() {
      m.jacobi_step(0, SIZEB/8, old_xs);
    });

    std::thread t1([&]() {
      m.jacobi_step(SIZEB/8, 2*SIZEB/8, old_xs);
    });

    std::thread t2([&]() {
      m.jacobi_step(2*SIZEB/8, 3*SIZEB/8, old_xs);
    });

    std::thread t3([&]() {
      m.jacobi_step(3*SIZEB/8, 4*SIZEB/8, old_xs);
    });

    std::thread t4([&]() {
      m.jacobi_step(4*SIZEB/8, 5*SIZEB/8, old_xs);
    });

    std::thread t5([&]() {
      m.jacobi_step(5*SIZEB/8, 6*SIZEB/8, old_xs);
    });

    std::thread t6([&]() {
      m.jacobi_step(6*SIZEB/8, 7*SIZEB/8, old_xs);
    });

    std::thread t7([&]() {
      m.jacobi_step(7*SIZEB/8, SIZEB, old_xs);
    });

    t0.join(); t2.join(); t4.join();
    t1.join(); t3.join(); t5.join();
    t6.join(); t7.join(); 
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






inline int parallel3 () {
  bool converged = false;
  int iters = 0;

  std::vector<double> old_xs = m.xs;

  auto time0 = std::chrono::high_resolution_clock::now();
  while (!converged&& iters < MAX_ITERS) {
    old_xs = m.xs;

    std::thread t1([&]() {
      m.jacobi_step(0, SIZEB/3, old_xs);
    });

    std::thread t2([&]() {
      m.jacobi_step(SIZEB/3, 2*SIZEB/3, old_xs);
    });

    std::thread t3([&]() {
      m.jacobi_step(2*SIZEB/3, SIZEB, old_xs);
    });

    t1.join(); t2.join(); t3.join();
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

