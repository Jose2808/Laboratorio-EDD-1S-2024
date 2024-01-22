public class listaSimple <E>{
    private nodo<E> head;

    public listaSimple() {
        this.head = null;
    }
    
    public void push(E valor){
        nodo<E> nodo = new nodo<E>(valor);

        if(this.head == null){
            this.head = nodo;

        }else{
            nodo.setSiguiente(this.head);
            this.head = nodo;
        }
    }

    public void insert(int index, E value){
        if(this.head == null || index == 0){
            push(value);

        }else{
            nodo<E> node = new nodo<E>(value);
            nodo<E> actual = this.head;
            int i = 0;

            while(actual.getSiguiente() != null && i < index - 1){
                actual = actual.getSiguiente();
                i++;
            }
            node.setSiguiente(actual.getSiguiente());
            actual.setSiguiente(node);
        }
    }

    public void append(E value){
        nodo<E> nodo = new nodo<E>(value);

        if(this.head == null){
            this.head = nodo;
        
        }else{
            nodo<E> temp = this.head;
            while(temp.getSiguiente() != null){
                temp = temp.getSiguiente();
            }
            temp.setSiguiente(nodo);
        }
    }

    public void delete(E value) {
        if(this.head == null){
            System.out.println("La lista se encuentra vacía");
            return;
        }

        if(this.head.getValor() == value){
            this.head = this.head.getSiguiente();
            System.out.println("El nodo se ha eliminado de forma correcta");

        }else{
            nodo<E> node = this.head;
            nodo<E> next = node.getSiguiente();

            while(next != null){
                if(next.getValor() == value){
                    node.setSiguiente(next.getSiguiente());
                    System.out.println("El nodo se ha eliminado de forma correcta");
                    return;
                }

                node = next;
                next = next.getSiguiente();
            }

            if(next == null){
                System.out.println("El nodo que se desea eliminar no existe");
            }
        }
    }

    public void printList() {
        nodo<E> nodo = this.head;

        while(nodo != null){
            System.out.println(nodo.getValor().toString());
            nodo = nodo.getSiguiente();
        }
        System.out.println();
    }
}
