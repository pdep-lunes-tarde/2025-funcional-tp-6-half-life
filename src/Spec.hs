module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

burgaPollo = Hamburguesa 100 [Pollo] 
burgaCarne = Hamburguesa 100 [Carne]
burgaConPan = Hamburguesa 100 [Pan, Carne, Cheddar]

correrTests :: IO ()
correrTests = hspec $ do
    describe "Parte 1" $ do
        it "Dada una hamburguesa de pollo, si se agranda, se agrega otro pollo. " $ do
            agrandar burgaPollo `shouldBe`  Hamburguesa 100 [Pollo, Pollo] 
        it "Dada una Hamburguesa de Carne, si se agranda, se agrega otra de Carne " $ do
            agrandar burgaCarne `shouldBe` Hamburguesa 100 [Carne, Carne]
        it "Dada una hamburguesa de Carne y Pollo, si se agranda, agrega carne" $ do
            agrandar burga1 `shouldBe` Hamburguesa 100 [Carne, Pollo, Carne]
        it "Dado un ingrediente se lo agrega a la Hamburguesa" $ do
            agregarIngredienteGenerico Cheddar burga1 `shouldBe` Hamburguesa 100 [Cheddar, Pollo, Carne]
        it "Dado un descuento, se lo aplica al precio base de la Hamburguesa" $ do
            descuento 10 burga1 `shouldBe` Hamburguesa 90 [Pollo, Carne]
        it "La PdeBurguer, es un CuartoDeLibra agrandado 2 veces, con Panceta Cheddar y un 20% de descuento, su precio final debe ser 96" $ do
            pdepBurger `shouldBe` Hamburguesa 96 [Cheddar,Panceta,Carne,Carne,Carne,Cheddar]
    describe "Parte 2" $ do
        it "Hamburguesa DobleCuarto, es un CuartoDeLibra con Carne y Cheddar, el precio final deberia ser 130" $ do
            dobleCuarto `shouldBe` Hamburguesa 130 [Cheddar,Carne,Carne,Cheddar]
        it "Hamburguesa bigPdeP, es un dobleCuarto con Curry el precio final deberia ser 135" $ do
            bigPdep `shouldBe` Hamburguesa 135 [Curry,Cheddar,Carne,Carne,Cheddar]
        it "Si una hamburguesa es del Dia, le agrega papas y un descuento del 30%" $ do
            delDia bigPdep `shouldBe` Hamburguesa 101.5 [Papas,Curry,Cheddar,Carne,Carne,Cheddar]
    describe "Parte 3" $ do
        it "Dada una hamburguesa, la convierte en veggie cambiandole determinados ingredientes" $ do
            hacerVeggie dobleCuarto `shouldBe` Hamburguesa 130 [QuesoDeAlmendras,PatiVegano,PatiVegano,QuesoDeAlmendras]
        it "Dada una hamburguesa con pan normal lo cambia por pan Integral" $ do
            hacerVeggie burgaConPan `shouldBe` Hamburguesa 100 [PanIntegral, PatiVegano, QuesoDeAlmendras]
            




