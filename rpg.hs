{-Definicao do tipo "Player" utilizando "sintaxe de registro" (cria funcoes automaticamente para acessar os valores que o tipo pode assumir) -}
data Player = Player { nomeDoJogador :: String
                     , hpDoJogador :: Integer
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
