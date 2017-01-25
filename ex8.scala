/********************************************************************************************************************
Linguagens de Programação - 2014/1 - Carlos Camarão                                                                                                                                                                                                                                  
Exercício 8: Traduzir o jogo PedraPapelTesoura de Haskell para Scala.

Alunos: Guilherme Saulo Alves e Mateus Lopes Teixeira

Observações: 
* 	As jogadas do computador foi feita randomicamente com a função scala.util.Random;
* 	Devido ao tempo corrido de final de semestre, não foi posivel mostrar as minhas jogadas e as jogadas do computador
  	como uma lista na amostra de resultados, em vez disso, as jogadas são mostradas ao longo do jogo;
*	O programa executa normalmente, porém esta ocorrendo um warning que não descobrimos o motivo

Comandos Usados na compilação:
scala ex8.scala ou
scalac ex8.scala
scala - classpath . PedraPapelTesoura 
*********************************************************************************************************************/

object PedraPapelTesoura {    
	
	class resultados (var jogadas:Int, var vitorias:Int, var derrotas:Int, var empates:Int){
		def jogada (amount:Int) {
			this.jogadas = this.jogadas + 1
		}
		def vitoria (amount:Int) {
			this.vitorias = this.vitorias + 1
		}
		def derrota (amount:Int) {
			this.derrotas = this.derrotas + 1
		}
		def empate (amount:Int) {
			this.empates = this.empates + 1
		}
	}

	def main (args: Array[String]){
		var fim = 0
		var resultados = new resultados (0,0,0,0)
		jogoInterativo
		
		def jogoInterativo () {
			var mao = ""
			while(fim != 1){
				print("\nDigite * (pedra), . (papel), ou x (tesoura): ")
				mao = readLine ()
				if (mao == "*" || mao == "pedra") {
					estrategia(0)
				}
				else if (mao == "." || mao == "papel") {
					estrategia(1)
				}
				else if (mao == "x" || mao == "tesoura") {
					estrategia(2)
				}
				else{
					fim = 1
					mostraResultado ();
				}            
			}
		}
	 
		def estrategia(minhaJogada:Int) {
			val rand = new scala.util.Random
			val suaJogada = rand.nextInt(3)
			jogo (minhaJogada, suaJogada)
		}

		def jogo(minhaJogada:Int, suaJogada:Int) {
			resultados jogada 1
			if (minhaJogada == 0 && suaJogada == 2) { 
				resultados vitoria 1
				println("Venci! Eu: Pedra x PC: Tesoura");
			}
			else if (minhaJogada == 1 && suaJogada == 0) {
				resultados vitoria 1
				println("Venci! Eu: Papel x PC: Pedra");
			}
			else if (minhaJogada == 2 && suaJogada == 1) {
				resultados vitoria 1
				println("Venci! Eu: Tesoura x PC: Papel");
			}
			else if (minhaJogada == 0 && suaJogada == 0) {
				resultados empate 1
				println("Empate! Eu: Pedra x PC: Pedra");
			}
			else if (minhaJogada == 1 && suaJogada == 1) {
				resultados empate 1
				println("Empate! Eu: Papel x PC: Papel");
			}
			else if (minhaJogada == 2 && suaJogada == 2) {
				resultados empate 1
				println("Empate! Eu: Tesoura x PC: Tesoura");
			}
			else if (minhaJogada == 0 && suaJogada == 1) { 
				resultados derrota 1
				println("Perdi! Eu: Pedra x PC: Papel");
			}
			else if (minhaJogada == 1 && suaJogada == 2) { 
				resultados derrota 1
				println("Perdi! Eu: Papel x PC: Tesoura");
			}
			else if (minhaJogada == 2 && suaJogada == 0) { 
				resultados derrota 1
				println("Perdi! Eu: Tesoura x PC: Pedra");
			}		           
		}

		def mostraResultado () {

			//println("Minhas Jogadas ")
			//println("SuasJogadas ")
				
			print("Houve " +resultados.jogadas+" jogada")
			if(resultados.jogadas > 1) println("s") else println("")
			print("Venci " +resultados.vitorias+" vez")
			if(resultados.vitorias > 1) println("es") else println("")
			print("Perdi " +resultados.derrotas+" vez")
			if(resultados.derrotas > 1) println("es") else println("")
			print("Houve " +resultados.empates+" empate")
			if(resultados.jogadas > 1) println("s") else println("")
				
			if(resultados.vitorias > resultados.derrotas)
				println("Venci o jogo!") 
			else if(resultados.vitorias == resultados.derrotas)
				println("Empate!")
			else if(resultados.vitorias < resultados.derrotas)
				println("Perdi o jogo, Computador Ganhou!")
		}
	}
}
