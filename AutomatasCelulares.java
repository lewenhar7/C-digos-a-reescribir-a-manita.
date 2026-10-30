/*
 * * Nombre de la clase: AutomatasCelulares
 * * Información de la versión: 4.0 (AutómatasCelulares)
 * * Autor: Oscar Leonardo Olvera Ruiz.
 * * Profesor: Manuel Alcántara Juárez.
 * * Ayudante: Ángel Renato Zamudio Malagón.
 * * Ayudante de laboratorio: Rodrigo Alejandro Sánchez Morales.
 * * Fecha: 26 de octubre de 2026.
 * * Copyright: © 2026 Olvera Ruiz Oscar Leonardo. Uso académico, Facultad de Ciencias, UNAM.
 */

import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.Scanner;

public class AutomatasCelulares {

    // ===========
    // CONSTANTES.
    // ===========

    /** Número de filas del tablero. */
    private static final int FILAS_TABLERO = 15;

    /** Número de columnas del tablero. */
    private static final int COLUMNAS_TABLERO = 30;

    /** Cantidad de generaciones que se sugiere simular (por ejemplo, 100 pasos). */
    private static final int GENERACIONES_SUGERIDAS = 100;

    /** Mínimo de generaciones que se pueden pedir. */
    private static final int GENERACIONES_MINIMAS = 1;

    /** Máximo de generaciones que se pueden pedir. */
    private static final int GENERACIONES_MAXIMAS = 500;

    /** Pausa, en milisegundos, entre una generación y la siguiente (si el usuario la pide). */
    private static final int PAUSA_ENTRE_GENERACIONES_MILISEGUNDOS = 250;

    /** Regla 1: con menos vecinos vivos que este valor, una celda viva muere por soledad. */
    private static final int VECINOS_MINIMOS_SUPERVIVENCIA = 2;

    /** Regla 3: con más vecinos vivos que este valor, una celda viva muere por sobrepoblación. */
    private static final int VECINOS_MAXIMOS_SUPERVIVENCIA = 3;

    /** Regla 4: cantidad exacta de vecinos vivos con la que una celda muerta nace. */
    private static final int VECINOS_PARA_NACER = 3;

    /** Símbolo con el que se imprime una celda viva. */
    private static final String SIMBOLO_CELDA_VIVA = "■";

    /** Símbolo con el que se imprime una celda muerta. */
    private static final String SIMBOLO_CELDA_MUERTA = "□";

    /** Carácter que marca una celda viva en la definición de los patrones (el punto es muerta). */
    private static final char MARCA_CELDA_VIVA_EN_PATRON = 'X';

    /** Opciones del menú principal. */
    private static final int OPCION_PATRON_PREDEFINIDO = 1;
    private static final int OPCION_CAPTURA_MANUAL = 2;
    private static final int OPCION_VER_REGLAS = 3;
    private static final int OPCION_SALIR = 4;

    /** Opciones del menú de patrones predefinidos. */
    private static final int OPCION_REGRESAR = 0;
    private static final int OPCION_BLOQUE = 1;
    private static final int OPCION_PARPADEADOR = 2;
    private static final int OPCION_SAPO = 3;
    private static final int OPCION_BALIZA = 4;
    private static final int OPCION_PLANEADOR = 5;
    private static final int OPCION_PENTOMINO_R = 6;

    /** Patrones predefinidos: "X" es una celda viva y "." es una celda muerta. */
    private static final String[] PATRON_BLOQUE = {"XX", "XX"};
    private static final String[] PATRON_PARPADEADOR = {"XXX"};
    private static final String[] PATRON_SAPO = {".XXX", "XXX."};
    private static final String[] PATRON_BALIZA = {"XX..", "XX..", "..XX", "..XX"};
    private static final String[] PATRON_PLANEADOR = {".X.", "..X", "XXX"};
    private static final String[] PATRON_PENTOMINO_R = {".XX", "XX.", ".X."};

    /** Posición inicial del planeador (cerca de la esquina superior izquierda, desde 0). */
    private static final int FILA_INICIAL_PLANEADOR = 1;
    private static final int COLUMNA_INICIAL_PLANEADOR = 1;

    /** Línea decorativa del encabezado. */
    private static final String LINEA_SEPARADORA = "==================================";

    /** Configuración regional para pasar a minúsculas respetando los acentos. */
    private static final Locale IDIOMA_ESPANOL = Locale.forLanguageTag("es-MX");

    // ===================
    // VARIABLES DE CLASE.
    // ===================

    /** Lector de la entrada estándar (teclado). */
    private static Scanner teclado = new Scanner(System.in);

    // ===================
    // PROGRAMA PRINCIPAL.
    // ===================

    /**
     * Punto de entrada: muestra el menú principal y lo repite hasta que el usuario elige salir.
     *
     * @param argumentos argumentos de la línea de comandos (no se utilizan).
     */
    public static void main(String[] argumentos) {
        int opcionMenu = 0;

        System.out.println(LINEA_SEPARADORA);
        System.out.println("   AUTÓMATAS CELULARES");
        System.out.println("   Juego de la vida de Conway");
        System.out.println(LINEA_SEPARADORA);

        try {
            // do-while: el menú se muestra al menos una vez y se repite hasta elegir "Salir".
            do {
                mostrarMenuPrincipal();
                opcionMenu = leerEntero("Seleccione una opción (" + OPCION_PATRON_PREDEFINIDO
                        + "-" + OPCION_SALIR + "): ", OPCION_PATRON_PREDEFINIDO, OPCION_SALIR);
                System.out.println();

                // switch: cada opción del menú ejecuta su procedimiento correspondiente.
                switch (opcionMenu) {
                    case OPCION_PATRON_PREDEFINIDO:
                        iniciarSimulacionConPatron();
                        break;
                    case OPCION_CAPTURA_MANUAL:
                        iniciarSimulacionManual();
                        break;
                    case OPCION_VER_REGLAS:
                        mostrarReglas();
                        break;
                    case OPCION_SALIR:
                        System.out.println("Fin del programa. ¡Hasta pronto!");
                        break;
                    default:
                        System.out.println("Opción no válida.");
                        break;
                }
            } while (opcionMenu != OPCION_SALIR);
        } catch (NoSuchElementException excepcion) {
            // Se cerró la entrada de datos (por ejemplo, con Ctrl+D): fin ordenado del programa.
            System.out.println();
            System.out.println("Se cerró la entrada de datos. Fin del programa.");
        } finally {
            teclado.close();
        }
    }

    // ===============
    // MENÚS Y REGLAS.
    // ===============

    /**
     * Imprime las opciones del menú principal.
     */
    private static void mostrarMenuPrincipal() {
        System.out.println();
        System.out.println("— MENÚ PRINCIPAL —");
        System.out.println(OPCION_PATRON_PREDEFINIDO + ". Simular con un patrón predefinido.");
        System.out.println(OPCION_CAPTURA_MANUAL + ". Simular capturando las celdas vivas.");
        System.out.println(OPCION_VER_REGLAS + ". Ver las reglas del juego.");
        System.out.println(OPCION_SALIR + ". Salir.");
    }

    /**
     * Imprime las opciones del menú de patrones predefinidos.
     */
    private static void mostrarMenuPatrones() {
        System.out.println("— PATRONES PREDEFINIDOS —");
        System.out.println(OPCION_BLOQUE + ". Bloque (vida estable).");
        System.out.println(OPCION_PARPADEADOR + ". Parpadeador (oscilador de periodo 2).");
        System.out.println(OPCION_SAPO + ". Sapo (oscilador de periodo 2).");
        System.out.println(OPCION_BALIZA + ". Baliza (oscilador de periodo 2).");
        System.out.println(OPCION_PLANEADOR + ". Planeador (se desplaza por el tablero).");
        System.out.println(OPCION_PENTOMINO_R + ". Pentominó R (evolución compleja).");
        System.out.println(OPCION_REGRESAR + ". Regresar al menú principal.");
    }

    /**
     * Imprime las reglas del juego y la simbología que usa el programa.
     */
    private static void mostrarReglas() {
        System.out.println("— REGLAS DEL JUEGO DE LA VIDA —");
        System.out.println("Cada celda observa a sus 8 vecinas (horizontales, verticales y"
                + " diagonales).");
        System.out.println("1. Soledad (subpoblación): una celda viva con menos de "
                + VECINOS_MINIMOS_SUPERVIVENCIA + " vecinos vivos muere.");
        System.out.println("2. Supervivencia (estabilidad): una celda viva con "
                + VECINOS_MINIMOS_SUPERVIVENCIA + " o " + VECINOS_MAXIMOS_SUPERVIVENCIA
                + " vecinos vivos sigue viva.");
        System.out.println("3. Sobrepoblación (masificación): una celda viva con más de "
                + VECINOS_MAXIMOS_SUPERVIVENCIA + " vecinos vivos muere.");
        System.out.println("4. Reproducción (nacimiento): una celda muerta con exactamente "
                + VECINOS_PARA_NACER + " vecinos vivos nace.");
        System.out.println("Simbología: " + SIMBOLO_CELDA_VIVA + " = celda viva, "
                + SIMBOLO_CELDA_MUERTA + " = celda muerta.");
        System.out.println("Todas las celdas cambian al mismo tiempo en cada generación.");
        System.out.println("Las celdas fuera del tablero se consideran muertas.");
    }

    // ===========================
    // ESTADO INICIAL DEL TABLERO.
    // ===========================

    /**
     * Pide al usuario un patrón predefinido, lo coloca en el tablero y ejecuta la simulación.
     */
    private static void iniciarSimulacionConPatron() {
        mostrarMenuPatrones();
        int opcionPatron = leerEntero("Seleccione un patrón (" + OPCION_REGRESAR + "-"
                + OPCION_PENTOMINO_R + "): ", OPCION_REGRESAR, OPCION_PENTOMINO_R);

        if (opcionPatron == OPCION_REGRESAR) {
            System.out.println("Se regresa al menú principal.");
        } else {
            boolean[][] tableroInicial = crearTableroConPatron(opcionPatron);
            ejecutarSimulacion(tableroInicial);
        }
    }

    /**
     * Pide al usuario las coordenadas de las celdas vivas y ejecuta la simulación con ellas.
     * Si no se captura ninguna celda viva no hay nada que simular.
     */
    private static void iniciarSimulacionManual() {
        boolean[][] tableroInicial = capturarCeldasVivas();

        if (contarCeldasVivas(tableroInicial) == 0) {
            System.out.println("No se capturó ninguna celda viva: no hay nada que simular.");
        } else {
            ejecutarSimulacion(tableroInicial);
        }
    }

    /**
     * Crea un tablero con todas las celdas muertas. Java inicializa en false todas las
     * posiciones de un arreglo boolean, por lo que no hace falta recorrerlo.
     *
     * @return tablero vacío de FILAS_TABLERO filas por COLUMNAS_TABLERO columnas.
     */
    private static boolean[][] crearTableroVacio() {
        return new boolean[FILAS_TABLERO][COLUMNAS_TABLERO];
    }

    /**
     * Crea un tablero y coloca en él el patrón que eligió el usuario.
     *
     * @param opcionPatron opción elegida en el menú de patrones (de 1 a 6).
     * @return tablero con el patrón colocado.
     */
    private static boolean[][] crearTableroConPatron(int opcionPatron) {
        boolean[][] tablero = crearTableroVacio();

        switch (opcionPatron) {
            case OPCION_BLOQUE:
                colocarPatronCentrado(tablero, PATRON_BLOQUE);
                break;
            case OPCION_PARPADEADOR:
                colocarPatronCentrado(tablero, PATRON_PARPADEADOR);
                break;
            case OPCION_SAPO:
                colocarPatronCentrado(tablero, PATRON_SAPO);
                break;
            case OPCION_BALIZA:
                colocarPatronCentrado(tablero, PATRON_BALIZA);
                break;
            case OPCION_PLANEADOR:
                // Va cerca de la esquina superior izquierda para que tenga espacio de avanzar.
                colocarPatron(tablero, PATRON_PLANEADOR, FILA_INICIAL_PLANEADOR,
                        COLUMNA_INICIAL_PLANEADOR);
                break;
            case OPCION_PENTOMINO_R:
                colocarPatronCentrado(tablero, PATRON_PENTOMINO_R);
                break;
            default:
                break;
        }
        return tablero;
    }

    /**
     * Coloca un patrón en el centro del tablero.
     *
     * @param tablero tablero donde se coloca el patrón.
     * @param patron  filas del patrón: "X" es una celda viva y "." es una celda muerta.
     */
    private static void colocarPatronCentrado(boolean[][] tablero, String[] patron) {
        int altoPatron = patron.length;
        int anchoPatron = patron[0].length();
        int filaInicial = (tablero.length - altoPatron) / 2;
        int columnaInicial = (tablero[0].length - anchoPatron) / 2;

        colocarPatron(tablero, patron, filaInicial, columnaInicial);
    }

    /**
     * Copia un patrón en el tablero a partir de una posición. Cada carácter "X" del patrón se
     * convierte en una celda viva; las celdas que quedarían fuera del tablero se ignoran.
     *
     * @param tablero        tablero donde se coloca el patrón.
     * @param patron         filas del patrón: "X" es una celda viva y "." es una celda muerta.
     * @param filaInicial    fila del tablero donde empieza el patrón (desde 0).
     * @param columnaInicial columna del tablero donde empieza el patrón (desde 0).
     */
    private static void colocarPatron(boolean[][] tablero, String[] patron, int filaInicial,
            int columnaInicial) {
        for (int fila = 0; fila < patron.length; fila++) {
            for (int columna = 0; columna < patron[fila].length(); columna++) {
                boolean esCeldaViva = patron[fila].charAt(columna) == MARCA_CELDA_VIVA_EN_PATRON;

                if (esCeldaViva && estaDentroDelTablero(tablero, filaInicial + fila,
                        columnaInicial + columna)) {
                    tablero[filaInicial + fila][columnaInicial + columna] = true;
                }
            }
        }
    }

    /**
     * Pide al usuario las celdas que estarán vivas al inicio. Cada celda se escribe como
     * "fila columna" y la captura termina cuando el usuario presiona Enter sin escribir nada.
     *
     * @return tablero con las celdas vivas capturadas.
     */
    private static boolean[][] capturarCeldasVivas() {
        boolean[][] tablero = crearTableroVacio();
        boolean capturaTerminada = false;

        System.out.println("— CAPTURA DE CELDAS VIVAS —");
        System.out.println("El tablero tiene " + FILAS_TABLERO + " filas y " + COLUMNAS_TABLERO
                + " columnas.");
        System.out.println("Escriba cada celda viva como: fila columna (por ejemplo: 5 7).");
        System.out.println("Cuando termine, presione Enter sin escribir nada.");

        while (!capturaTerminada) {
            String entrada = leerLinea("Celda viva (fila columna): ");

            if (entrada.isEmpty()) {
                capturaTerminada = true;
            } else {
                marcarCeldaViva(tablero, entrada);
            }
        }
        return tablero;
    }

    /**
     * Interpreta una línea escrita por el usuario ("fila columna", separadas por espacios o por
     * una coma), valida las coordenadas y, si son correctas, marca esa celda como viva.
     *
     * @param tablero tablero donde se marca la celda.
     * @param entrada texto capturado por el usuario.
     */
    private static void marcarCeldaViva(boolean[][] tablero, String entrada) {
        String[] partes = entrada.split("[\\s,]+");
        boolean formatoValido = partes.length == 2 && esNumeroEntero(partes[0])
                && esNumeroEntero(partes[1]);

        if (!formatoValido) {
            System.out.println("Error: escriba dos números enteros separados por un espacio.");
        } else {
            int fila = Integer.parseInt(partes[0]);
            int columna = Integer.parseInt(partes[1]);

            if (fila < 1 || fila > tablero.length || columna < 1 || columna > tablero[0].length) {
                System.out.println("Error: la fila debe estar entre 1 y " + tablero.length
                        + " y la columna entre 1 y " + tablero[0].length + ".");
            } else if (tablero[fila - 1][columna - 1]) {
                System.out.println("La celda (" + fila + ", " + columna + ") ya estaba viva.");
            } else {
                // El usuario cuenta desde 1 y el arreglo desde 0, por eso se resta 1.
                tablero[fila - 1][columna - 1] = true;
                System.out.println("Celda (" + fila + ", " + columna + ") marcada como viva.");
            }
        }
    }

    // ===========
    // SIMULACIÓN.
    // ===========

    /**
     * Muestra el estado inicial, pide cuántas generaciones simular y las calcula y muestra una
     * por una. Si la población se extingue la simulación se detiene, porque un tablero sin
     * celdas vivas ya no puede cambiar.
     *
     * @param tableroInicial tablero con el estado inicial (generación 0).
     */
    private static void ejecutarSimulacion(boolean[][] tableroInicial) {
        boolean[][] tablero = tableroInicial;
        boolean poblacionExtinta = false;
        int generacionesCalculadas = 0;

        System.out.println();
        imprimirTablero(tablero, 0);

        int totalGeneraciones = leerEntero("Número de generaciones a simular ("
                + GENERACIONES_MINIMAS + "-" + GENERACIONES_MAXIMAS + ", por ejemplo "
                + GENERACIONES_SUGERIDAS + "): ", GENERACIONES_MINIMAS, GENERACIONES_MAXIMAS);
        boolean conPausa = leerSiNo("¿Desea una pausa entre generaciones? (s/n): ");
        System.out.println();

        // Ciclo del tiempo: cada vuelta calcula y muestra una generación nueva.
        for (int generacion = 1; generacion <= totalGeneraciones && !poblacionExtinta;
                generacion++) {
            if (conPausa) {
                pausar(PAUSA_ENTRE_GENERACIONES_MILISEGUNDOS);
            }

            // El tablero viejo se reemplaza por el tablero nuevo que devuelve la función.
            tablero = calcularSiguienteGeneracion(tablero);
            generacionesCalculadas = generacion;

            imprimirTablero(tablero, generacion);
            poblacionExtinta = contarCeldasVivas(tablero) == 0;
        }

        imprimirResumen(generacionesCalculadas, contarCeldasVivas(tablero));
    }

    /**
     * Calcula la siguiente generación. Las reglas se aplican a todas las celdas al mismo
     * tiempo: cada celda se evalúa con el tablero actual (que no se modifica) y su nuevo estado
     * se escribe en un tablero temporal, para que los datos nuevos de una celda no afecten a
     * sus vecinas.
     *
     * @param tableroActual tablero de la generación actual (no se modifica).
     * @return tablero temporal con la generación siguiente.
     */
    private static boolean[][] calcularSiguienteGeneracion(boolean[][] tableroActual) {
        int filas = tableroActual.length;
        int columnas = tableroActual[0].length;
        boolean[][] tableroTemporal = new boolean[filas][columnas];

        for (int fila = 0; fila < filas; fila++) {
            for (int columna = 0; columna < columnas; columna++) {
                int vecinosVivos = contarVecinosVivos(tableroActual, fila, columna);

                if (tableroActual[fila][columna]) {
                    // La celda está viva.
                    if (vecinosVivos < VECINOS_MINIMOS_SUPERVIVENCIA) {
                        tableroTemporal[fila][columna] = false; // Regla 1: Soledad.
                    } else if (vecinosVivos <= VECINOS_MAXIMOS_SUPERVIVENCIA) {
                        tableroTemporal[fila][columna] = true;  // Regla 2: Supervivencia.
                    } else {
                        tableroTemporal[fila][columna] = false; // Regla 3: Sobrepoblación.
                    }
                } else {
                    // La celda está muerta.
                    if (vecinosVivos == VECINOS_PARA_NACER) {
                        tableroTemporal[fila][columna] = true;  // Regla 4: Reproducción.
                    } else {
                        tableroTemporal[fila][columna] = false; // Sigue muerta.
                    }
                }
            }
        }
        return tableroTemporal;
    }

    /**
     * Cuenta los vecinos vivos de una celda: revisa las 8 posiciones que la rodean
     * (horizontales, verticales y diagonales) y no cuenta a la celda misma. Los vecinos que
     * quedan fuera del tablero se consideran muertos.
     *
     * @param tablero tablero donde se cuentan los vecinos.
     * @param fila    fila de la celda (desde 0).
     * @param columna columna de la celda (desde 0).
     * @return cantidad de vecinos vivos (de 0 a 8).
     */
    private static int contarVecinosVivos(boolean[][] tablero, int fila, int columna) {
        int vecinosVivos = 0;

        for (int desplazamientoFila = -1; desplazamientoFila <= 1; desplazamientoFila++) {
            for (int desplazamientoColumna = -1; desplazamientoColumna <= 1;
                    desplazamientoColumna++) {
                boolean esLaCeldaCentral = desplazamientoFila == 0 && desplazamientoColumna == 0;
                int filaVecina = fila + desplazamientoFila;
                int columnaVecina = columna + desplazamientoColumna;

                if (!esLaCeldaCentral && estaDentroDelTablero(tablero, filaVecina, columnaVecina)
                        && tablero[filaVecina][columnaVecina]) {
                    vecinosVivos++;
                }
            }
        }
        return vecinosVivos;
    }

    /**
     * Indica si una posición pertenece al tablero.
     *
     * @param tablero tablero que se revisa.
     * @param fila    fila de la posición (desde 0).
     * @param columna columna de la posición (desde 0).
     * @return true si la posición está dentro de los bordes del tablero.
     */
    private static boolean estaDentroDelTablero(boolean[][] tablero, int fila, int columna) {
        return fila >= 0 && fila < tablero.length && columna >= 0 && columna < tablero[0].length;
    }

    /**
     * Cuenta cuántas celdas vivas hay en todo el tablero.
     *
     * @param tablero tablero que se revisa.
     * @return cantidad de celdas vivas.
     */
    private static int contarCeldasVivas(boolean[][] tablero) {
        int celdasVivas = 0;

        for (int fila = 0; fila < tablero.length; fila++) {
            for (int columna = 0; columna < tablero[fila].length; columna++) {
                if (tablero[fila][columna]) {
                    celdasVivas++;
                }
            }
        }
        return celdasVivas;
    }

    // ===================
    // SALIDA EN PANTALLA.
    // ===================

    /**
     * Imprime el tablero actual y su estado: número de generación, celdas vivas y el estado de
     * cada celda (viva o muerta) con la simbología de la práctica.
     *
     * @param tablero    tablero que se imprime.
     * @param generacion número de generación (0 es la configuración inicial).
     */
    private static void imprimirTablero(boolean[][] tablero, int generacion) {
        if (generacion == 0) {
            System.out.println("— CONFIGURACIÓN INICIAL (GENERACIÓN 0) —");
        } else {
            System.out.println("— GENERACIÓN " + generacion + " —");
        }
        System.out.println("Celdas vivas: " + contarCeldasVivas(tablero));

        for (int fila = 0; fila < tablero.length; fila++) {
            StringBuilder lineaTablero = new StringBuilder();

            for (int columna = 0; columna < tablero[fila].length; columna++) {
                if (tablero[fila][columna]) {
                    lineaTablero.append(SIMBOLO_CELDA_VIVA);
                } else {
                    lineaTablero.append(SIMBOLO_CELDA_MUERTA);
                }
                if (columna < tablero[fila].length - 1) {
                    lineaTablero.append(" ");
                }
            }
            System.out.println(lineaTablero);
        }
        System.out.println();
    }

    /**
     * Imprime el resumen que se muestra al terminar una simulación.
     *
     * @param generacionesCalculadas cantidad de generaciones que se calcularon.
     * @param celdasVivas            celdas vivas que quedaron en la última generación.
     */
    private static void imprimirResumen(int generacionesCalculadas, int celdasVivas) {
        System.out.println("— FIN DE LA SIMULACIÓN —");
        System.out.println("Generaciones calculadas: " + generacionesCalculadas);
        System.out.println("Celdas vivas al final: " + celdasVivas);

        if (celdasVivas == 0) {
            System.out.println("La población se extinguió: el tablero se quedó sin celdas vivas.");
        }
    }

    /**
     * Detiene la ejecución un tiempo para poder ver cada generación con calma.
     *
     * @param milisegundos duración de la pausa.
     */
    private static void pausar(int milisegundos) {
        try {
            Thread.sleep(milisegundos);
        } catch (InterruptedException excepcion) {
            Thread.currentThread().interrupt();
        }
    }

    // =================
    // LECTURA DE DATOS.
    // =================

    /**
     * Muestra un mensaje y lee una línea completa del teclado, sin espacios al inicio ni al final.
     *
     * @param mensaje texto que se muestra al usuario.
     * @return línea capturada.
     */
    private static String leerLinea(String mensaje) {
        System.out.print(mensaje);
        return teclado.nextLine().trim();
    }

    /**
     * Lee un número entero dentro de un rango. Repite la pregunta mientras el dato no sea un
     * número entero o esté fuera del rango.
     *
     * @param mensaje texto que se muestra al usuario.
     * @param minimo  valor mínimo permitido (incluido).
     * @param maximo  valor máximo permitido (incluido).
     * @return número entero válido.
     */
    private static int leerEntero(String mensaje, int minimo, int maximo) {
        int valor = 0;
        boolean valorValido = false;

        while (!valorValido) {
            String entrada = leerLinea(mensaje);
            if (esNumeroEntero(entrada)) {
                valor = Integer.parseInt(entrada);
                if (valor >= minimo && valor <= maximo) {
                    valorValido = true;
                } else {
                    System.out.println("Error: ingrese un número entre " + minimo + " y "
                            + maximo + ".");
                }
            } else {
                System.out.println("Error: debe ingresar un número entero"
                        + " (sin letras ni símbolos).");
            }
        }
        return valor;
    }

    /**
     * Hace una pregunta de sí o no y repite la pregunta hasta recibir una respuesta válida.
     *
     * @param mensaje pregunta que se muestra al usuario.
     * @return true si la respuesta es sí (s, si); false si es no (n, no).
     */
    private static boolean leerSiNo(String mensaje) {
        boolean respuesta = false;
        boolean respuestaValida = false;

        while (!respuestaValida) {
            String entrada = leerLinea(mensaje).toLowerCase(IDIOMA_ESPANOL);

            if (entrada.equals("s") || entrada.equals("si") || entrada.equals("sí")) {
                respuesta = true;
                respuestaValida = true;
            } else if (entrada.equals("n") || entrada.equals("no")) {
                respuesta = false;
                respuestaValida = true;
            } else {
                System.out.println("Error: responda con s (sí) o n (no).");
            }
        }
        return respuesta;
    }

    /**
     * Indica si un texto se puede convertir a número entero.
     *
     * @param texto texto que se revisa.
     * @return true si el texto representa un número entero.
     */
    private static boolean esNumeroEntero(String texto) {
        boolean esEntero = true;

        try {
            Integer.parseInt(texto);
        } catch (NumberFormatException excepcion) {
            esEntero = false;
        }
        return esEntero;
    }
}
