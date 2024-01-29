import ListaEnlazada.animal;

public class App {
    public static void main(String[] args) throws Exception {
        listaSimple<animal> listasimple = new listaSimple<animal>();
        listasimple.push(new animal("Felino", "Michi", 5));
        listasimple.push(new animal("Canino", "Firulais", 10));

        listasimple.printList();
    }
}
