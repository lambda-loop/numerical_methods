
#include <iostream>
#include <thread>
#include <vector>

#include <matrix.h>

#define SIZE   10
#define TIMES 100

auto m = matrix(SIZE);

inline void serial () {
    bool convergiu = false;
    int iteracoes = 0;
    std::vector<double> old_xs;

    // int controler = amortizer;
    auto inicio_tempo = std::chrono::high_resolution_clock::now();

    while (!convergiu && iteracoes < max_iter) {
        
        old_xs = jacobi_aserial(m);
        convergiu = testar_convergencia(old_xs, m.xs, tolerancia);
        // iteracoes+= amortizer;
    }

    // Para o cronômetro
    auto fim_tempo = std::chrono::high_resolution_clock::now();
    
    // Calcula a duração em segundos
    std::chrono::duration<double> duracao = fim_tempo - inicio_tempo;

    // Imprime o relatório
    std::cout << "--- Relatório de Execução ---\n";
    if (convergiu) {
        std::cout << "Status: Convergiu com sucesso!\n";
    } else {
        std::cout << "Status: ALERTA! Atingiu o limite de " << max_iter << " iterações sem convergir.\n";
    }
    std::cout << "Iterações: " << iteracoes << "\n";
    std::cout << "Tempo gasto: " << duracao.count() << " segundos.\n";
    std::cout << "-----------------------------\n";
    return duracao.count();
  



}


int main () {

  



}


