#include <chrono>
#include <algorithm>
#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <iomanip>

#define TIMES 10
// todo: std::array~std::array instead
struct matrix {
  std::vector<double> xs;
  std::vector<std::vector<double>> as;
  std::vector<double> bs;
  int size;
};

matrix mkmatrix (std::vector<std::vector<double>> m) {
  int size = m.size() - 1;

  std::vector<std::vector<double>> as;
  for (int i = 0; i < size; i++) {
    std::vector<double> a_iline;
    for (int j = 0; j < size; j++) {
      a_iline.push_back(m[i][j]);
    }
    as.push_back(a_iline);
  }

  std::vector<double> bs;
  for (int i = 0; i < size; i++) {
    bs.push_back(m[i][size+1]);
  }
  
  std::vector<double> xs;
  for (int i = 0; i < size; i++) {
    xs.push_back(0.0);
    // xs.push_back(bs[i]/as[i][i]);
  }
  return matrix {
    xs, as, bs, size
  };
}


std::vector<double> jacobi_aserial(matrix &m) {
  int size = m.size;

  auto &as = m.as;
  auto &bs = m.bs;

  auto xs1 = m.xs;
  auto xs2 = m.xs;

  for (int j = 0; j < TIMES; j++) {
    for(int i = 0; i < size; i++) {
      double aii = as[i][i];
      double sum = bs[i];
      for (int j = 0; j < size; j++) {
        if (i!=j) {
          sum -= as[i][j] * xs1[j];
        }
      }

      xs2[i] = sum/aii;
    }

    for(int i = 0; i < size; i++) {
      double aii = as[i][i];
      double sum = bs[i];
      for (int j = 0; j < size; j++) {
        if (i!=j) {
          sum -= as[i][j] * xs2[j];
        }
      }

      xs1[i] = sum/aii;
    }
  }

  m.xs = std::move(xs1);
  return xs2;
}



void jacobi_serial(matrix &m) {
  int size = m.size;

  auto old_xs = m.xs;
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


void serial(matrix *m) {
  int size = m->size;
  for(int i = 0; i < size; i++) {
    double aii = m->as[i][i];
    // double bi  = m->bs[i];
    double sum = m->bs[i];
    for (int j = 0; j < size; j++) {
      if(i!=j) {
        sum -= m->as[i][j] * m->xs[j];
      }
    }
    m->xs[i] = sum/aii;
  }
}

std::vector<std::vector<double>> gerar_matriz_dominante(int n) {
    // Inicializa matriz n linhas por n+1 colunas com zeros
    std::vector<std::vector<double>> m(n, std::vector<double>(n + 1, 0.0));
    
    // Configuração do gerador de números aleatórios modernos do C++
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(-100000.0, 10000.0); // Apenas números positivos!
    // std::uniform_real_distribution<double> dis(-10.0, 10.0); // Valores entre -10 e 10

    // Margem de segurança para garantir a dominância (evitando o sumiço do 1e-15)
    double margem = 1e-15; 

    for (int i = 0; i < n; ++i) {
        double soma_linha = 0.0;

        // Passo 1: Preenche a linha com números aleatórios (incluindo o b)
        for (int j = 0; j < n + 1; ++j) {
            m[i][j] = dis(gen);
            
            // Passo 2: Soma os valores absolutos, ignorando a diagonal e a coluna 'b'
            if (i != j && j < n) {
                soma_linha += std::abs(m[i][j]);
            }
        }

        // Passo 3: Força a diagonal a ser estritamente dominante
        // a_ii = soma(|a_ij|) + margem
        // Usamos std::copysign para manter o sinal original do número na diagonal (opcional, mas legal)
        double valor_dominante = soma_linha + margem;
        m[i][i] = std::copysign(valor_dominante, m[i][i]); 
    }

    return m;
}

// Função utilitária apenas para imprimir a matriz no terminal
void imprimir_matriz(const std::vector<std::vector<double>>& m) {
    int n = m.size();
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n + 1; ++j) {
            std::cout << std::setw(10) << std::setprecision(4) << m[i][j] << " ";
            if (j == n - 1) std::cout << "| "; // Separador visual para o 'b'
        }
        std::cout << "\n";
    }
}

// Verifica se a maior diferença entre os x antigos e novos é menor que a tolerância
bool testar_convergencia(const std::vector<double>& x_antigo, const std::vector<double>& x_novo, double tolerancia) {
    double maior_diferenca = 0.0;
    
    for (int i = 0; i < x_novo.size(); i++) {
        double diferenca_atual = std::abs(x_novo[i] - x_antigo[i]);
        if (diferenca_atual > maior_diferenca) {
            maior_diferenca = diferenca_atual;
        }
    }
    
    return maior_diferenca < tolerancia;
}

double resolver_ate_convergir_(matrix& m
                             , double tolerancia = 1e-9
                             , int max_iter = 10000) {
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

double resolver_ate_convergir(matrix& m, double tolerancia = 1e-9, int max_iter = 10000) {
    bool convergiu = false;
    int iteracoes = 0;
    std::vector<double> x_antigo = m.xs; // Guarda o estado inicial

    // Inicia o cronômetro
    auto inicio_tempo = std::chrono::high_resolution_clock::now();

    while (!convergiu && iteracoes < max_iter) {
        x_antigo = m.xs; // Salva os valores antes de atualizar
        
        jacobi_serial(m);       // Roda 1 iteração do seu método (modifica m.xs)
        
        convergiu = testar_convergencia(x_antigo, m.xs, tolerancia);
        iteracoes++;
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
