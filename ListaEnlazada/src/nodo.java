public class nodo<E> {
    private E valor;
    private nodo<E> siguiente;

    public nodo(E valor) {
        this.valor = valor;
        this.siguiente = null;
    }

    public E getValor() {
        return valor;
    }

    public nodo<E> getSiguiente() {
        return siguiente;
    }

    public void setValor(E valor) {
        this.valor = valor;
    }

    public void setSiguiente(nodo<E> siguiente) {
        this.siguiente = siguiente;
    }
}
