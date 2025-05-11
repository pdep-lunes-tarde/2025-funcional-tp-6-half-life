module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconDeTofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente BaconDeTofu = 10
precioIngrediente PanIntegral = 3


data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

burga1 :: Hamburguesa
burga1 = Hamburguesa 100 [Pollo, Carne] 

--PARTE 1 :: Hamburguesas

{- cada vez que se agranda una hamburguesa se agrega otro ingrediente base (por ahora, son Carne o Pollo),
se elige el ingrediente base a agregar según lo que ya haya en la hamburguesa
(si había carne se agrega carne, si había pollo se agrega pollo, si había ambos da igual cuál se agregue). -}

agrandar :: Hamburguesa -> Hamburguesa
agrandar burga
    |tieneCarne burga && tienePollo burga = agregarCarne burga --como tiene los 2 llamo a cualquier funcion agregar
    |tieneCarne burga = agregarCarne burga 
    |otherwise= agregarPollo burga

tieneCarne :: Hamburguesa -> Bool
tieneCarne = any (== Carne) . ingredientes

tienePollo :: Hamburguesa -> Bool
tienePollo = any (== Pollo) . ingredientes

agregarCarne :: Hamburguesa -> Hamburguesa
agregarCarne = agregarIngredientes (Carne :)

agregarPollo :: Hamburguesa -> Hamburguesa
agregarPollo = agregarIngredientes (Pollo :)

agregarIngredientes :: ([Ingrediente]->[Ingrediente]) -> Hamburguesa ->Hamburguesa
agregarIngredientes func burga = burga { ingredientes = func(ingredientes burga) } 

agregarIngredienteGenerico :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngredienteGenerico ingrediente burga = burga { ingredientes = ingrediente : ingredientes burga}

sumarPrecioIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
sumarPrecioIngrediente ingrediente burga = burga { precioBase = precioBase burga + precioIngrediente ingrediente}

-- agregarIngrediente: recibe un ingrediente y una hambrugesa lo agrega a la hamburguesa.

--funcion con composicion y aplicacion parcial, le pasas el ingrediente y la hamburguesa, y llama a agregarIngredientes
agregarIngredienteGenerico' :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngredienteGenerico' ingrediente = agregarIngredientes (ingrediente:)

--quiero ir desde una hamburguesa a otra pero con el precio descontado
descuento :: Number -> Hamburguesa -> Hamburguesa
descuento desc burga = 
    (cambiarPrecioBase burga . negate) desc

-- Si en lugar de un + fuese un menos en la cuenta del precioBase, solo esa funcion bastaria,
-- pero queria hacer composicion, por eso use el negate y esta otra funcion.
cambiarPrecioBase :: Hamburguesa -> Number -> Hamburguesa
cambiarPrecioBase burga desc = burga { 
    precioBase = precioBase burga + ((desc*precioBase burga) / 100)
}

--la pdepBurger, que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento.
-- Su precio final deberia ser 110.  (¿¿¿es un caso de prueba para los tests??)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 100 [Carne,Cheddar]

pdepBurger = descuento 20 . agregarIngredienteYPrecio Cheddar . agregarIngredienteYPrecio Panceta . agrandar . agrandar $ cuartoDeLibra


--PARTE 2: Algunas hamburguesas mas.
--En esta parte 2 lo que pense fue generalizar mas la funcion de agrandar, para que no se limite con carne y pollo
--y ademas, que sume el precio del ingrediente al precioBase.

agregarIngredienteYPrecio :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngredienteYPrecio ingrediente = 
    agregarIngredienteGenerico ingrediente . sumarPrecioIngrediente ingrediente 

agrandarGenerico :: [Ingrediente] -> Hamburguesa -> Hamburguesa
agrandarGenerico ingredientes burga = foldl (flip agregarIngredienteYPrecio) burga ingredientes

-- dobleCuarto = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.

dobleCuarto :: Hamburguesa
dobleCuarto = agrandarGenerico[Carne, Cheddar] cuartoDeLibra

-- es un doble cuarto con curry. El precio final deberia ser 89
bigPdep :: Hamburguesa
bigPdep = agregarIngredienteYPrecio Curry dobleCuarto

{-
elDia = es una promo que, dada una hamburguesa, le agrega Papas y un descuento del 30%.
Por ejemplo, podría pedir una big pdep del dia
y debería ser como una big pdep (doble cuarto con curry) pero con papas y el descuento del 30%.
Por ejemplo una doble cuarto del día deberia valer 88.
-}

delDia :: Hamburguesa -> Hamburguesa
delDia burga = 
    descuento 30 . agregarIngredienteYPrecio Papas $ burga

-- PARTE 3: Algunos cambios mas

{-
hacerVeggie : cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano
(nuevo ingrediente base, también de precio 10), el cheddar lo cambia por queso de almendras
y la panceta por bacon de tofu.
-}

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = agregarIngredientes (map reemplazarIngrediente)

reemplazarIngrediente :: Ingrediente -> Ingrediente
reemplazarIngrediente Carne = PatiVegano
reemplazarIngrediente Pollo = PatiVegano
reemplazarIngrediente Cheddar = QuesoDeAlmendras
reemplazarIngrediente Panceta = BaconDeTofu 
reemplazarIngrediente Pan = PanIntegral









