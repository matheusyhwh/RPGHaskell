{-Definicao do tipo "Player" utilizando "sintaxe de registro" (cria funcoes automaticamente para acessar os valores que o tipo pode assumir) -}
data Player = Player { nomeDoJogador :: String
                     , hpDoJogador :: Int
                     , hpMaxDoJogador :: Int
                     , velocidadeDoJogador :: Int
                     , expDoJogador :: Int
                     , ataqueDoJogador :: Int
                     , danoMagicoDoJogador :: Int
                     , defesaDoJogador :: Int
                     , defesaMagicaDoJogador :: Int
                     , controlNivelDoJogador :: Int {-Quando os pontos de experiencia do jogador atigem o valor dessa variavel o jogador passa de nivel-}
                     , nivelDoJogador :: Int
                     , manaDoJogador :: Int
                     , manaMaxDoJogador :: Int
                     , jogadorIsAlive :: Bool {-Indica se o jogador esta vivo ou nao-}
                     } deriving (Show)  {-Para o tipo passar a fazer parte da typeclass show-}
{-Definicao do tipo "Aliado"-}

data Aliado = Aliado { nomeDoAliado :: String
                     , hpDoAliado :: Int
                     , ataqueDoAliado :: Int
                     , danoMagicoDoAliado :: Int
                     , defesaDoAliado :: Int
                     , defesaMagicaDoAliado :: Int
                     , velocidadeDoAliado :: Int
                     , manaDoAliado :: Int
                     , manaMaxDoAliado :: Int
                     , aliadoIsAlive :: Int
                     , levelDoAliado :: Int
                     } deriving (Show)

data Inimigo = Inimigo { nomeDoInimigo :: String
                       , hpDoInimigo :: Int
                       , velocidadeDoInimigo :: Int
                       , ataqueDoInimigo :: Int
                       , defesaDoInimigo :: Int
                       , defesaMagicaDoInimigo :: Int
                       , xpDropDoInimigo :: Int {-Esse atributo indica quanto de xp o inimigo dara ao ser derrotado-}
                       , inimigoIsAlive :: Bool
                       } deriving (Show)

{-Funcao que recebe o nome do player e o cria com valores padrao-}
criaPlayer :: String -> Player
criaPlayer nome = Player nome 300 300 15 0 10 20 10 15 100 1 200 200 True
{-Cria um Inimigo (Apenas teste) -}
criaInimigo :: String -> Inimigo
criaInimigo nome = Inimigo nome 300 300 15 0 10 20  True



{-Adiciona exp em um player passado no parametro -}
countExp :: Int -> Player -> Player
countExp exp j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) ((expDoJogador j) + (exp)) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)


{-Antigo "gerenciadorDeExp" ~ Altera caracteristicas do jogador quando o mesmo sobe de nivel ~ Retorna um novo player-}
updatePlayerIfItsNeeded :: Player -> Player
updatePlayerIfItsNeeded jogador
  | (expDoJogador jogador) >= (controlNivelDoJogador jogador) = setManaToMax (setHpToMax (updateControlNivelJogador (updateNivelJogador (updateDefesaMagicaJogador (updateDefesaJogador (updateAtaqueJogador (updateVelocidadeJogador (updateManaMaxJogador (updateHpMaxJogador jogador)))))))))
  | otherwise = jogador



{-INÍCIO: METODOS AUXILIARES ~ Usados em "updatePlayerIfItsNeeded"-}

{-*Obs: Importante usar `div` ao inves de "/" para quando se quer dividir 2 Int e se quer o resultado em Int-}
updateHpMaxJogador :: Player -> Player
updateHpMaxJogador j = Player (nomeDoJogador j) (hpDoJogador j) (((hpMaxDoJogador j)`div`5) + hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

updateManaMaxJogador :: Player -> Player
updateManaMaxJogador j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) ((manaMaxDoJogador j) + ((manaMaxDoJogador j)`div`5)) (jogadorIsAlive j)

updateVelocidadeJogador :: Player -> Player
updateVelocidadeJogador j =  Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) ((velocidadeDoJogador j) + ((velocidadeDoJogador j)`div`3)) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

updateAtaqueJogador :: Player -> Player
updateAtaqueJogador j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) ((ataqueDoJogador j) + ((ataqueDoJogador j)`div`5)) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

updateDefesaJogador :: Player -> Player
updateDefesaJogador j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) ((defesaDoJogador j) + ((defesaDoJogador j)`div`5)) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

updateDefesaMagicaJogador :: Player -> Player
updateDefesaMagicaJogador j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) ((defesaMagicaDoJogador j) + ((defesaMagicaDoJogador j) `div` 5)) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

updateNivelJogador :: Player -> Player
updateNivelJogador j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) ((nivelDoJogador j) + 1) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

updateControlNivelJogador :: Player -> Player
updateControlNivelJogador j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) ((controlNivelDoJogador j) * 2) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

{-Deixa o player full de hp-}
setHpToMax :: Player -> Player
setHpToMax j = Player (nomeDoJogador j) (hpMaxDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

{-Deixa o player full de mana-}
setManaToMax :: Player -> Player
setManaToMax j = Player (nomeDoJogador j) (hpDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaMaxDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

{-Funcoes que realizam os calculos do dano fisico e do dano magico abaixo-}
calculaDanoFisico :: Double -> Double -> Double
calculaDanoFisico dano defesa = (dano / ((defesa/100) + 1))

calculaDanoMagico :: Double -> Double -> Double
calculaDanoMagico danoMagico defesaMagica = (danoMagico / ((defesaMagica / 100) + 1))

{-Atualiza o hp do player quando recebe o dano do inimigo, retornando um novo player com hp atualizado e morto se for o caso (recebe o dano e o jogador)-}
receiveDamageByEnemy :: Int -> Player -> Player
receiveDamageByEnemy dano j
    | (((hpDoJogador j) - dano) <= 0) = Player (nomeDoJogador j) ((hpDoJogador j) - dano) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (False)
    | otherwise = Player (nomeDoJogador j) ((hpDoJogador j) - dano) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) (manaDoJogador j) (manaMaxDoJogador j) (jogadorIsAlive j)

{-Funcao que cura um personagem: "Funcao que enche o hp do personagem ao custo de mana"-}
curaPlayer :: Player -> Player
curaPlayer j
    | (totalDeCura) + (hpDoJogador j) >= hpMaxDoJogador j = Player (nomeDoJogador j) (hpMaxDoJogador j) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) ((manaDoJogador j) - (10 * (nivelDoJogador j))) (manaMaxDoJogador j) (jogadorIsAlive j)
    | otherwise = Player (nomeDoJogador j) ((hpDoJogador j) + totalDeCura) (hpMaxDoJogador j) (velocidadeDoJogador j) (expDoJogador j) (ataqueDoJogador j) (danoMagicoDoJogador j) (defesaDoJogador j) (defesaMagicaDoJogador j) (controlNivelDoJogador j) (nivelDoJogador j) ((manaDoJogador j) - (10 * (nivelDoJogador j))) (manaMaxDoJogador j) (jogadorIsAlive j)
    where totalDeCura = (hpMaxDoJogador j) `div` 2 {-Usar `div` sempre que tiver uma divisao de inteiros e se quiser um resultado inteiro-}

printSeparator = putStrLn "-------------------------------------------------------------"


printAcoes = do
                putStrLn ""
                printSeparator
                putStrLn "Selecione uma ação :"
                putStrLn "1 - Atague fisico "
                putStrLn "2 - Cura  "
                putStrLn "3 - Magia "
                printSeparator
                putStrLn "Digite o numero referente a ação desejada : "

printEscolhas :: Int -> IO()
printEscolhas 1 = putStrLn "Você ataca o inimigo fisicamente"
printEscolhas 2 = putStrLn "Você sente que precisa recuperar suas energias"
printEscolhas 3 = putStrLn "Você resolve chamar as forças aliadas"
printEscolhas x = putStrLn "Opção invalida : por favor selecione uma opção valida "

receiveDamageByPlayer :: Int -> Inimigo -> Inimigo
receiveDamageByPlayer dano i
  | (((hpDoInimigo i) - dano) <= 0) = Inimigo (nomeDoInimigo i) ((hpDoInimigo i)-dano) (velocidadeDoInimigo i) (ataqueDoInimigo i) (defesaDoInimigo i) (defesaMagicaDoInimigo i) (xpDropDoInimigo i) (False)
  | otherwise = Inimigo (nomeDoInimigo i) ((hpDoInimigo i)-dano) (velocidadeDoInimigo i) (ataqueDoInimigo i) (defesaDoInimigo i) (defesaMagicaDoInimigo i) (xpDropDoInimigo i) (inimigoIsAlive i)


{-Nesse caso, nao estamos tratando ainda as multiplas escolhas, estamos fazendo "de conta" que a escolha é sempre a numero 1 (atacar inimigo)-}
gerenciadorDeEscolhas n jogador inimigo = do
  printEscolhas 1
  return (jogador, receiveDamageByPlayer (ataqueDoJogador jogador) inimigo)


--seletorDeAcoes :: Int -> Int -> Int
seletorDeAcoes j defesa defesaMagica = do
     printAcoes

     entrada <- getLine
     let escolha = read entrada :: Int
     putStr ""
     printEscolhas escolha



battleManager jogador inimigo
  | ((hpDoJogador jogador) <= 0) = do
    print $ "Jogador morto!"
    return False
  | ((hpDoInimigo inimigo) <= 0) = do
    print $ "Inimigo morto!"
    return True
  | otherwise = do
    printAcoes
    x <- getLine
    let escolha = read x :: Int
    a <- gerenciadorDeEscolhas escolha jogador inimigo
    let novoPlayer = fst a :: Player
    let novoInimigo = snd a :: Inimigo

    battleManager novoPlayer novoInimigo


--Compara um unico atributo de player e inimigo, os valores ja sao dados em inteiros. Retorna +1 Se o player ganhou. Se o inimigo ganhou, retorna -1
comparaAtributo :: Int -> Int -> Int
comparaAtributo atributoPlayer atributoInimigo
  | (atributoPlayer >= atributoInimigo) = 1
  | otherwise = -1

printInfoGuerreiro :: IO()
printInfoGuerreiro =  putStrLn "1 - Guerreiro: Guerreiros combinam força, liderança e vasto conhecimento em armas e armaduras para criar o caos no campo de batalha."

printInfoPaladino :: IO()
printInfoPaladino = putStrLn "2 - Paladino: Estes guerreiros sagrados estão equipados com armaduras de placas para enfrentar os inimigos mais perigosos e são adeptos da benção da Luz que lhes permite curar feridas."

printInfoCacador :: IO()
printInfoCacador = putStrLn "3 - Caçador: Apesar de suas armas à distância serem extremamente eficientes, os caçadores se encontram em desvantagem quando seus inimigos aproximam para o combate corpo a corpo."

printInfoMago :: IO()
printInfoMago = putStrLn "4 - Mago: Apesar de dominarem poderosas magias ofensivas, os magos são frágeis e usam armaduras leves deixando-os particularmente vulneráveis contra ataques corpo a corpo"

printClasses :: IO()
printClasses = do
  putStrLn ""
  putStrLn "------------- Escolha a classe que seu personagem irá pertencer -------------"
  putStrLn ""
  printInfoGuerreiro
  putStrLn ""
  printInfoPaladino
  putStrLn ""
  printInfoCacador
  putStrLn ""
  printInfoMago
  putStrLn "-----------------------------------------------------------------------------"
  putStrLn ""

-- Compara atributos do player e do inimigo (ataque, defesa, defesaMagica e velocidade) - Se o resultado for negativo: Inimigo ganha. Se for positivo: Player ganha a luta
comparaTodosOsAtributos :: Player -> Inimigo -> Int
comparaTodosOsAtributos j i = (comparaAtributo (ataqueDoJogador j) (ataqueDoInimigo i)) + (comparaAtributo(defesaDoJogador j) (defesaDoInimigo i)) + (comparaAtributo(defesaMagicaDoJogador j) (defesaMagicaDoInimigo i)) + (comparaAtributo(velocidadeDoJogador j) (velocidadeDoInimigo i))

--Imprime todas as comparacoes dado um player e um inimigo
printComparacoes :: Player -> Inimigo -> IO()
printComparacoes j i = do
  putStrLn ""
  putStrLn ("---------------- " ++ (nomeDoJogador j) ++ " x " ++ (nomeDoInimigo i) ++ " ----------------")
  putStrLn ""
  printComparacaoAtaque j i
  printComparacaoDefesa j i
  printComparacaoDefesaMagica j i
  printComparacaoVelocidade j i
  putStrLn ""
  putStrLn "-------------------------------------------------------------------"
  putStrLn ""


--Abaixo, métodos auxiliares que comparam e imprimem a comparacao de atributos entre player e inimigo, usados em printComparacoes!!
printComparacaoAtaque :: Player -> Inimigo -> IO()
printComparacaoAtaque j i
  | (comparaAtributo (ataqueDoJogador j) (ataqueDoInimigo i)) == 1 = putStrLn ("ATAQUE: " ++ show (ataqueDoJogador j) ++ " >= " ++ show (ataqueDoInimigo i))
  | otherwise = putStrLn ("ATAQUE: " ++ show (ataqueDoJogador j) ++ " < " ++ show (ataqueDoInimigo i))

printComparacaoDefesa :: Player -> Inimigo -> IO()
printComparacaoDefesa j i
  | (comparaAtributo (defesaDoJogador j) (defesaDoInimigo i)) == 1 = putStrLn ("DEFESA: " ++ show (defesaDoJogador j) ++ " >= " ++ show (defesaDoInimigo i))
  | otherwise = putStrLn ("DEFESA: " ++ show (defesaDoJogador j) ++ " < " ++ show (defesaDoInimigo i))

printComparacaoDefesaMagica :: Player -> Inimigo -> IO()
printComparacaoDefesaMagica j i
  | (comparaAtributo (defesaMagicaDoJogador j) (defesaMagicaDoInimigo i)) == 1 = putStrLn ("DEFESA MÁGICA: " ++ show (defesaMagicaDoJogador j) ++ " >= " ++ show (defesaMagicaDoInimigo i))
  | otherwise = putStrLn ("DEFESA MÁGICA: " ++ show (defesaMagicaDoJogador j) ++ " < " ++ show (defesaMagicaDoInimigo i))

printComparacaoVelocidade :: Player -> Inimigo -> IO()
printComparacaoVelocidade j i
  | (comparaAtributo (velocidadeDoJogador j) (velocidadeDoInimigo i)) == 1 = putStrLn ("VELOCIDADE" ++ show (velocidadeDoJogador j) ++ " >= " ++ show (velocidadeDoInimigo i))
  | otherwise = putStrLn ("VELOCIDADE: "++ show (velocidadeDoJogador j) ++ " < " ++ show (velocidadeDoInimigo i))
















{-- REFACTORING THIS
gerenciadorDeEscolhas 1 jogador inimigo = do
  printEscolhas 1
  let a = receiveDamageByPlayer (ataqueDoJogador jogador) inimigo
  return (jogador, a)











--seletorDeAcoes :: Int -> Int -> Int
seletorDeAcoes j defesa defesaMagica = do
     printAcoes

     entrada <- getLine
     let escolha = read entrada :: Int
     putStr ""
     printEscolhas escolha


batleManager jogador inimigo
    | ((hpDoJogador jogador) <= 0) = do
      putStrLn ""
      return False
    | ((hpDoInimigo inimigo) <= 0) = do
      putStrLn ""
      return False
    | otherwise = do
        printAcoes
        x <- getLine
        let escolha = read x :: Int
        let a = gerenciadorDeEscolhas escolha jogador inimigo
        let novoJogador = (fst a)
        let novoInimigo = (snd a)
        return batleManager (fst a) (snd a)











main =  do

  batleManager (hpDoJogador j) (manaDoJogador j) j i  (hpDoInimigo i)--}
