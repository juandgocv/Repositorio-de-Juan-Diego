//Estructuras de Datos
//20 de Noviembre de 2024
//Santiago Hernandez, Isaias Acosta, Julian Diaz, Juan Carreno

#include <iostream>
#include <string>
#include <list>
#include <vector>
#include <fstream>
#include <limits>
#include <algorithm>
#include <cmath>
#include <sstream>
#include <queue>

using namespace std;

// Estructura que representa un objeto 3D con nombre, vértices y caras
struct Objeto3D {
    string nombre;                          // Nombre del objeto
    vector<vector<float>> vertices;         // Lista de vértices representados como coordenadas (x, y, z)
    vector<vector<int>> caras;              // Lista de caras representadas por índices de vértices
};

// Lista global para almacenar los objetos cargados en memoria
list<Objeto3D> objetosEnMemoria;

// Declaración de funciones utilizadas en el programa
void mostrarAyuda();
void mostrarAyudaComando(const string& comando);
void cargarNombreArchivo(const string& nombre_archivo);
void listado();
void generarEnvolvente(const string& nombre_objeto);
void generarEnvolventeGlobal();
void descargarNombreObjeto(const string& nombre_objeto);
void guardarNombreObjeto(const string& nombre_objeto, const string& nombre_archivo);
void v_cercano(float px, float py, float pz, const string& nombre_objeto);
void v_cercano(float px, float py, float pz);
void v_cercanos_caja(const string& nombre_objeto);
vector<int> dijkstra(const vector<vector<float>>& distancias, int inicio, int fin, float& longitud_total);
vector<vector<float>> calcularMatrizDistancias(const Objeto3D& objeto);
void ruta_corta(int i1, int i2, const string& nombre_objeto);
vector<float> calcularCentroide(const Objeto3D& objeto);
void ruta_corta_centro(int i1, const string& nombre_objeto);

// Busca un objeto en la lista global por su nombre
list<Objeto3D>::iterator buscarObjeto(const string& nombre) {
    return find_if(objetosEnMemoria.begin(), objetosEnMemoria.end(),
        [&nombre](const Objeto3D& obj) {
            return obj.nombre == nombre; // Compara el nombre del objeto con el buscado
        });
}

// Calcula la distancia euclidiana entre dos puntos 3D
float calcularDistanciaEuclidiana(const vector<float>& punto1, const vector<float>& punto2) {
    return sqrt(pow(punto1[0] - punto2[0], 2) + pow(punto1[1] - punto2[1], 2) + pow(punto1[2] - punto2[2], 2));
}

int main() {
    string input, nombre_archivo, nombre_objeto; // Variables para entrada de datos
    int opcion; // Opción seleccionada por el usuario
    float px, py, pz; // Coordenadas para puntos 3D
    int p1, p2; // Índices de puntos para operaciones de rutas
    size_t pos1, pos2; // Posiciones en cadenas para manejo de entrada

    cout << "Bienvenido al sistema de manipulacion de mallas 3D.\n Escribe 'ayuda' para ver los comandos disponibles." << endl;

    while (true) {
        // Menú principal del sistema
        cout << "\nOpciones disponibles: " << endl;
        cout << "   1. Cargar nombre archivo." << endl;
        cout << "   2. Listado." << endl;
        cout << "   3. Generar envolvente de un objeto." << endl;
        cout << "   4. Generar envolvente global." << endl;
        cout << "   5. Descargar nombre objeto." << endl;
        cout << "   6. Guardar objeto." << endl;
        cout << "   7. Salir." << endl;
        cout << "   8. Ayuda." << endl;
        cout << "   9. Vertice mas cercano a un punto (por objeto)." << endl;
        cout << "  10. Vertice mas cercano a un punto (todos los objetos)." << endl;
        cout << "  11. Vertices mas cercanos a caja envolvente." << endl;
        cout << "  12. Vertice ruta corta"<<endl;
        cout << "  13. Vertice ruta corta centro"<<endl;
        cout << "Elige una opcion $ ";
        getline(cin, input);

        if (input == "ayuda") {
            mostrarAyuda(); // Muestra ayuda general
            continue;
        } else if (input.find("ayuda") == 0) {
            string comando = input.substr(6); // Extrae el comando después de "ayuda"
            mostrarAyudaComando(comando);
            continue;
        }

        try {
            opcion = stoi(input); // Convierte la entrada en un número
        } catch (const invalid_argument&) {
            cout << "Opcion no valida." << endl;
            continue;
        }

        switch (opcion) {
        case 1:
            cout << "Introduce el nombre del archivo para cargar el objeto$ ";
            getline(cin, nombre_archivo);
            cargarNombreArchivo(nombre_archivo); // Llama a la función para cargar el archivo
            break;
        case 2:
            listado(); // Lista los objetos en memoria
            break;
        case 3:
            cout << "Introduce el nombre del objeto para generar su envolvente$ ";
            getline(cin, nombre_objeto);
            generarEnvolvente(nombre_objeto); // Genera la envolvente de un objeto específico
            break;
        case 4:
            generarEnvolventeGlobal(); // Genera la envolvente de todos los objetos
            break;
        case 5:
            cout << "Introduce el nombre del objeto a descargar$ ";
            getline(cin, nombre_objeto);
            descargarNombreObjeto(nombre_objeto); // Elimina un objeto de memoria
            break;
        case 6:
            cout << "Introduce el nombre del objeto a guardar$ ";
            getline(cin, nombre_objeto);
            cout << "Introduce el nombre del archivo de destino$ ";
            getline(cin, nombre_archivo);
            guardarNombreObjeto(nombre_objeto, nombre_archivo); // Guarda un objeto en un archivo
            break;
        case 7:
            cout << "Saliendo del programa." << endl;
            return 0;
        case 8:
            mostrarAyuda(); // Muestra la ayuda general
            break;
        case 9:
            cout << "Introduce el punto (px,py,pz) seguido del nombre del objeto$ ";
            getline(cin, input);

            // Procesa la entrada para extraer coordenadas x, y, z
            input.erase(remove(input.begin(), input.end(), ' '), input.end()); // Elimina espacios
            pos1 = input.find(',');
            pos2 = input.find(',', pos1 + 1);

            if (pos1 == string::npos || pos2 == string::npos) {
                cout << "Formato incorrecto. Asegurate de usar el formato x,y,z." << endl;
                break;
            }

            try {
                px = stof(input.substr(0, pos1));
                py = stof(input.substr(pos1 + 1, pos2 - pos1 - 1));
                pz = stof(input.substr(pos2 + 1));
            } catch (const invalid_argument&) {
                cout << "Error: Formato de punto invalido." << endl;
                break;
            }

            cout << "Introduce el nombre del objeto$ ";
            getline(cin, nombre_objeto);
            v_cercano(px, py, pz, nombre_objeto); // Busca el vértice más cercano en el objeto
            break;
        case 10:
            // Solicita al usuario las coordenadas de un punto para buscar el vértice más cercano en todos los objetos
            cout << "Introduce el punto (px,py,pz) para buscar el vertice mas cercano entre todos los objetos$ ";
            getline(cin, input);

            // Remover espacios y dividir por comas
            input.erase(remove(input.begin(), input.end(), ' '), input.end());  // Eliminar espacios
            pos1 = input.find(',');
            pos2 = input.find(',', pos1 + 1);

            // Validar formato del punto ingresado
            if (pos1 == string::npos || pos2 == string::npos) {
                cout << "Formato incorrecto. Asegurate de usar el formato x,y,z." << endl;
                break;
            }

            try {
                // Convertir las coordenadas ingresadas a valores numéricos
                px = stof(input.substr(0, pos1));
                py = stof(input.substr(pos1 + 1, pos2 - pos1 - 1));
                pz = stof(input.substr(pos2 + 1));
            } catch (const invalid_argument&) {
                cout << "Error: Formato de punto invalido." << endl;
                break;
            }

            // Llama a la función para buscar el vértice más cercano en todos los objetos
            v_cercano(px, py, pz);
            break;

        case 11:
            // Solicita al usuario el nombre de un objeto
            cout << "Introduce el nombre del objeto para buscar los vertices mas cercanos a la caja envolvente$ ";
            getline(cin, nombre_objeto);

            // Llama a la función para buscar vértices cercanos a la caja envolvente del objeto
            v_cercanos_caja(nombre_objeto);
            break;

        case 12:
            // Solicita al usuario dos puntos representados por índices para calcular una ruta corta
            cout << "Introduce el punto 1 y punto 2 seguido por una coma (p1,p2)$";
            getline(cin, input);

            // Remover espacios y dividir por comas
            input.erase(remove(input.begin(), input.end(), ' '), input.end());  // Eliminar espacios
            pos1 = input.find(',');

            // Validar formato del par de índices ingresado
            if (pos1 == string::npos) {
                cout << "Formato incorrecto. Asegurate de usar el formato p1,p2." << endl;
                break;
            }

            try {
                // Convertir los índices ingresados a valores numéricos
                p1 = stof(input.substr(0, pos1));
                p2 = stof(input.substr(pos1 + 1));
            } catch (const invalid_argument&) {
                cout << "Error: Formato de punto invalido." << endl;
                break;
            }

            // Solicita al usuario el nombre del objeto para el cálculo de la ruta
            cout << "Introduce el nombre del objeto$ ";
            getline(cin, nombre_objeto);

            // Llama a la función para calcular la ruta más corta entre dos puntos en el objeto
            ruta_corta(p1, p2, nombre_objeto);
            break;

        case 13:
            // Solicita al usuario un punto representado por un índice para calcular la ruta corta desde el centroide
            cout << "Introduce el punto deseado$ ";
            getline(cin, input);

            // Remover espacios y procesar el índice ingresado
            input.erase(remove(input.begin(), input.end(), ' '), input.end());  // Eliminar espacios
            pos1 = input.find(',');

            try {
                // Convertir el índice ingresado a un valor numérico
                p1 = stof(input.substr(0, pos1));
            } catch (const invalid_argument&) {
                cout << "Error: Formato de punto invalido." << endl;
                break;
            }

            // Solicita al usuario el nombre del objeto para el cálculo de la ruta
            cout << "Introduce el nombre del objeto$ ";
            getline(cin, nombre_objeto);

            // Llama a la función para calcular la ruta más corta desde el centroide hacia el punto dado
            ruta_corta_centro(p1, nombre_objeto);
            break;

        default:
            // Manejo de opciones inválidas ingresadas por el usuario
            cout << "Opcion no valida." << endl;
            break;

		}
	}

	return 0;
}

// Implementación de la función para encontrar el vértice más cercano a un punto en un objeto específico
void v_cercano(float px, float py, float pz, const string& nombre_objeto) {
    // Buscar el objeto en la lista de objetos cargados en memoria
    auto it = buscarObjeto(nombre_objeto);
    if (it == objetosEnMemoria.end()) {
        // Mensaje de error si el objeto no se encuentra
        cout << "Error: El objeto " << nombre_objeto << " no ha sido cargado en memoria." << endl;
        return;
    }

    // Referencia al objeto encontrado
    const auto& objeto = *it;

    // Variables para rastrear la distancia mínima y el vértice más cercano
    float distancia_min = numeric_limits<float>::max();
    int vertice_mas_cercano = -1;

    // Iterar sobre todos los vértices del objeto
    for (size_t i = 0; i < objeto.vertices.size(); ++i) {
        // Calcular la distancia euclidiana entre el vértice actual y el punto dado
        float distancia = calcularDistanciaEuclidiana(objeto.vertices[i], {px, py, pz});
        if (distancia < distancia_min) {
            distancia_min = distancia;
            vertice_mas_cercano = i; // Actualizar el índice del vértice más cercano
        }
    }

    // Imprimir el vértice más cercano y su información
    const auto& vertice = objeto.vertices[vertice_mas_cercano];
    cout << "El vertice " << vertice_mas_cercano << " (" << vertice[0] << ", " << vertice[1] << ", " << vertice[2] << ") del objeto "
         << nombre_objeto << " es el mas cercano al punto (" << px << ", " << py << ", " << pz << "), a una distancia de " << distancia_min << "." << endl;
}

// Implementación para buscar el vértice más cercano en todos los objetos cargados en memoria
void v_cercano(float px, float py, float pz) {
    // Verificar si la memoria está vacía
    if (objetosEnMemoria.empty()) {
        cout << "Memoria vacia: Ningun objeto ha sido cargado en memoria." << endl;
        return;
    }

    // Variables para rastrear el vértice y el objeto más cercanos
    float distancia_min = numeric_limits<float>::max();
    string objeto_mas_cercano;
    int vertice_mas_cercano = -1;

    // Iterar sobre todos los objetos y sus vértices
    for (const auto& objeto : objetosEnMemoria) {
        for (size_t i = 0; i < objeto.vertices.size(); ++i) {
            // Calcular la distancia euclidiana entre cada vértice y el punto dado
            float distancia = calcularDistanciaEuclidiana(objeto.vertices[i], {px, py, pz});
            if (distancia < distancia_min) {
                distancia_min = distancia;
                vertice_mas_cercano = i;
                objeto_mas_cercano = objeto.nombre; // Guardar el nombre del objeto más cercano
            }
        }
    }

    // Recuperar el objeto más cercano y mostrar resultados
    auto it = buscarObjeto(objeto_mas_cercano);
    if (it == objetosEnMemoria.end()) {
        cout << "Error: No se encontro un objeto mas cercano." << endl;
        return;
    }

    const auto& vertice = it->vertices[vertice_mas_cercano];
    cout << "El vertice " << vertice_mas_cercano << " (" << vertice[0] << ", " << vertice[1] << ", " << vertice[2] << ") del objeto "
         << objeto_mas_cercano << " es el mas cercano al punto (" << px << ", " << py << ", " << pz << "), a una distancia de " << distancia_min << "." << endl;
}

// Implementación para encontrar los vértices más cercanos a las esquinas de la caja envolvente
void v_cercanos_caja(const string& nombre_objeto) {
    // Buscar el objeto en la lista de objetos cargados en memoria
    auto it = buscarObjeto(nombre_objeto);
    if (it == objetosEnMemoria.end()) {
        cout << "Error: El objeto " << nombre_objeto << " no ha sido cargado en memoria." << endl;
        return;
    }

    const auto& objeto = *it;

    // Inicializar valores para calcular las esquinas de la caja envolvente
    vector<float> pmin(3, numeric_limits<float>::max()); // Coordenadas mínimas
    vector<float> pmax(3, numeric_limits<float>::lowest()); // Coordenadas máximas

    // Calcular la caja envolvente
    for (const auto& vertice : objeto.vertices) {
        for (int i = 0; i < 3; ++i) {
            pmin[i] = min(pmin[i], vertice[i]); // Actualizar coordenada mínima
            pmax[i] = max(pmax[i], vertice[i]); // Actualizar coordenada máxima
        }
    }

    // Definir las esquinas de la caja envolvente
    vector<vector<float>> esquinas_caja = {
        {pmin[0], pmin[1], pmin[2]}, {pmax[0], pmin[1], pmin[2]},
        {pmin[0], pmax[1], pmin[2]}, {pmax[0], pmax[1], pmin[2]},
        {pmin[0], pmin[1], pmax[2]}, {pmax[0], pmin[1], pmax[2]},
        {pmin[0], pmax[1], pmax[2]}, {pmax[0], pmax[1], pmax[2]}
    };

    // Encontrar el vértice más cercano a cada esquina de la caja envolvente
    cout << "Vertices mas cercanos a las esquinas de la caja envolvente del objeto " << nombre_objeto << ":" << endl;
    for (size_t i = 0; i < esquinas_caja.size(); ++i) {
        float distancia_min = numeric_limits<float>::max();
        int vertice_mas_cercano = -1;

        for (size_t j = 0; j < objeto.vertices.size(); ++j) {
            float distancia = calcularDistanciaEuclidiana(objeto.vertices[j], esquinas_caja[i]);
            if (distancia < distancia_min) {
                distancia_min = distancia;
                vertice_mas_cercano = j;
            }
        }

        // Imprimir los resultados para cada esquina
        const auto& vertice = objeto.vertices[vertice_mas_cercano];
        cout << "Esquina " << i + 1 << " (" << esquinas_caja[i][0] << ", " << esquinas_caja[i][1] << ", " << esquinas_caja[i][2] << ") -> "
             << "Vertice " << vertice_mas_cercano << " (" << vertice[0] << ", " << vertice[1] << ", " << vertice[2] << ") a una distancia de " << distancia_min << endl;
    }
}

void cargarNombreArchivo(const string& nombre_archivo) {
	ifstream archivo(nombre_archivo);  // Abrir archivo

	if (!archivo.is_open()) {
		cout << "Error: No se pudo abrir el archivo " << nombre_archivo << ". Verifica si el archivo existe y tiene permisos de lectura." << endl;
		return;
	}

	string nombre_objeto;
	while (archivo >> nombre_objeto) {
		if (nombre_objeto == "-1") {
			// Fin de la lectura de un objeto
			break;
		}

		// Leer el numero de vertices
		int n_vertices;
		archivo >> n_vertices;

		if (archivo.fail()) {
			cout << "Error al leer el nC:mero de vC)rtices del objeto " << nombre_objeto << endl;
			archivo.close();
			return;
		}

		Objeto3D nuevoObjeto;
		nuevoObjeto.nombre = nombre_objeto;
		nuevoObjeto.vertices.resize(n_vertices, vector<float>(3));

		// Leer los vertices
		for (int i = 0; i < n_vertices; ++i) {
			archivo >> nuevoObjeto.vertices[i][0] >> nuevoObjeto.vertices[i][1] >> nuevoObjeto.vertices[i][2];
			if (archivo.fail()) {
				cout << "Error al leer los vC)rtices del objeto " << nombre_objeto << endl;
				archivo.close();
				return;
			}
		}

		// Leer las caras (hasta encontrar un -1)
		vector<int> cara;
		while (true) {
			int n_caras;
			archivo >> n_caras;

			if (archivo.fail() || n_caras == -1) {
				// Terminamos de leer las caras del objeto
				break;
			}

			cara.resize(n_caras);
			for (int i = 0; i < n_caras; ++i) {
				archivo >> cara[i];
			}

			nuevoObjeto.caras.push_back(cara);
		}

		objetosEnMemoria.push_back(nuevoObjeto);
		cout << "Objeto " << nombre_objeto << " cargado exitosamente." << endl;
	}

	archivo.close();  // Cerrar el archivo
}


void listado() {
    // Verifica si no hay objetos cargados en memoria
    if (objetosEnMemoria.empty()) {
        // Muestra un mensaje indicando que no hay objetos
        cout << "No hay objetos cargados en memoria." << endl;
    } else {
        // Muestra un listado de los objetos cargados
        cout << "Objetos en memoria: " << endl;
        for (const auto& obj : objetosEnMemoria) {
            // Imprime el nombre del objeto y el número de vértices y caras
            cout << obj.nombre << " (vertices: " << obj.vertices.size()
                 << ", caras: " << obj.caras.size() << ")" << endl;
        }
    }
}

void generarEnvolvente(const string& nombre_objeto) {
    // Busca el objeto por nombre en la lista de objetos en memoria
    auto it = buscarObjeto(nombre_objeto);
    if (it == objetosEnMemoria.end()) {
        // Si no se encuentra, muestra un mensaje de error
        cout << "Error: El objeto no esta cargado en memoria." << endl;
        return;
    }

    const auto& objeto = *it; // Referencia al objeto encontrado

    // Inicializa los límites mínimos y máximos para calcular la envolvente
    vector<float> pmin(3, numeric_limits<float>::max());
    vector<float> pmax(3, numeric_limits<float>::lowest());

    // Recorre los vértices del objeto para determinar los límites
    for (const auto& vertice : objeto.vertices) {
        for (int i = 0; i < 3; ++i) {
            // Calcula el mínimo y máximo en cada dimensión
            pmin[i] = min(pmin[i], vertice[i]);
            pmax[i] = max(pmax[i], vertice[i]);
        }
    }

    // Crea un nuevo objeto 3D para la caja envolvente
    Objeto3D cajaEnvolvente;
    cajaEnvolvente.nombre = "env_" + nombre_objeto; // Asigna un nombre a la envolvente

    // Define los vértices de la caja envolvente
    cajaEnvolvente.vertices = {
        {pmin[0], pmin[1], pmin[2]},
        {pmax[0], pmin[1], pmin[2]},
        {pmin[0], pmax[1], pmin[2]},
        {pmax[0], pmax[1], pmin[2]},
        {pmin[0], pmin[1], pmax[2]},
        {pmax[0], pmin[1], pmax[2]},
        {pmin[0], pmax[1], pmax[2]},
        {pmax[0], pmax[1], pmax[2]}
    };
    
	cajaEnvolvente.caras = {
		{0, 1, 3, 2},
		{4, 5, 7, 6},
		{0, 1, 5, 4},
		{2, 3, 7, 6},
		{0, 2, 6, 4},
		{1, 3, 7, 5}
	};

	objetosEnMemoria.push_back(cajaEnvolvente);
	cout << "Caja envolvente del objeto " << nombre_objeto << " generada exitosamente." << endl;
}

void generarEnvolventeGlobal() {
	if (objetosEnMemoria.empty()) {
		cout << "No hay objetos en memoria para generar una envolvente global." << endl;
		return;
	}

	vector<float> pmin(3, numeric_limits<float>::max());
	vector<float> pmax(3, numeric_limits<float>::lowest());

	for (const auto& objeto : objetosEnMemoria) {
		for (const auto& vertice : objeto.vertices) {
			for (int i = 0; i < 3; ++i) {
				pmin[i] = min(pmin[i], vertice[i]);
				pmax[i] = max(pmax[i], vertice[i]);
			}
		}
	}

	Objeto3D cajaEnvolventeGlobal;
	cajaEnvolventeGlobal.nombre = "envolvente_global";
	cajaEnvolventeGlobal.vertices = {
		{pmin[0], pmin[1], pmin[2]},
		{pmax[0], pmin[1], pmin[2]},
		{pmin[0], pmax[1], pmin[2]},
		{pmax[0], pmax[1], pmin[2]},
		{pmin[0], pmin[1], pmax[2]},
		{pmax[0], pmin[1], pmax[2]},
		{pmin[0], pmax[1], pmax[2]},
		{pmax[0], pmax[1], pmax[2]}
	};

	cajaEnvolventeGlobal.caras = {
		{0, 1, 3, 2},
		{4, 5, 7, 6},
		{0, 1, 5, 4},
		{2, 3, 7, 6},
		{0, 2, 6, 4},
		{1, 3, 7, 5}
	};

	objetosEnMemoria.push_back(cajaEnvolventeGlobal);
	cout << "Caja envolvente global generada exitosamente." << endl;
}

void descargarNombreObjeto(const string& nombre_objeto) {
	auto it = buscarObjeto(nombre_objeto);
	if (it == objetosEnMemoria.end()) {
		cout << "Error: El objeto no esta cargado en memoria." << endl;
		return;
	} 
}
void guardarNombreObjeto(const string& nombre_objeto, const string& nombre_archivo) {
    // Busca el objeto en memoria por su nombre utilizando la función `buscarObjeto`.
    auto it = buscarObjeto(nombre_objeto);

    // Verifica si el objeto no está en memoria.
    if (it == objetosEnMemoria.end()) {
        // Si no se encuentra, muestra un mensaje de error y finaliza la función.
        cout << "Error: El objeto no esta en memoria." << endl;
        return;
    }

    // Obtiene una referencia constante al objeto encontrado.
    const auto& objeto = *it;

    // Intenta abrir un archivo para escritura con el nombre proporcionado.
    ofstream archivo(nombre_archivo);
    if (!archivo.is_open()) {
        // Si el archivo no puede abrirse, muestra un mensaje de error y finaliza la función.
        cout << "Error: No se pudo abrir el archivo de destino." << endl;
        return;
    }

    // Escribe la cantidad de vértices del objeto en la primera línea del archivo.
    archivo << objeto.vertices.size() << endl;

    // Recorre todos los vértices del objeto.
    for (const auto& vertice : objeto.vertices) {
        // Escribe las coordenadas (x, y, z) de cada vértice en el archivo, separadas por espacios.
        archivo << vertice[0] << " " << vertice[1] << " " << vertice[2] << endl;
    }

    // Escribe la cantidad de caras del objeto en el archivo.
    archivo << objeto.caras.size() << endl;

    // Recorre todas las caras del objeto.
    for (const auto& cara : objeto.caras) {
        // Escribe primero la cantidad de índices (vértices) que componen la cara.
        archivo << cara.size();
        
        // Luego, escribe los índices de los vértices que forman la cara, separados por espacios.
        for (int indice : cara) {
            archivo << " " << indice;
        }
        
        // Al final de cada cara, añade un salto de línea.
        archivo << endl;
    }
}
void mostrarAyudaComando(const string& comando) {
	if (comando == "cargar nombre archivo") {
		cout << "Cargar nombre archivo: Carga un objeto 3D desde un archivo. Usa el comando 'cargar nombre archivo'." << endl;
	} else if (comando == "listado") {
		cout << "Listado: Muestra una lista de los objetos 3D en memoria. Usa el comando 'listado'." << endl;
	} else if (comando == "generar envolvente de un objeto") {
		cout << "Generar envolvente de un objeto: Genera una caja envolvente para un objeto 3D especifico. Usa el comando 'generar envolvente de un objeto'." << endl;
	} else if (comando == "generar envolvente global") {
		cout << "Generar envolvente global: Genera una caja envolvente para todos los objetos en memoria. Usa el comando 'generar envolvente global'." << endl;
	} else if (comando == "descargar nombre objeto") {
		cout << "Descargar nombre objeto: Elimina un objeto 3D de la memoria. Usa el comando 'descargar nombre objeto'." << endl;
	} else if (comando == "guardar objeto") {
		cout << "Guardar objeto: Guarda un objeto 3D en un archivo. Usa el comando 'guardar objeto'." << endl;}
		else if (comando == "ruta corta") {
			cout << "Identifica los indices de los vertices que conforman la ruta mas corta entre los vertices dados para el objeto nombre_objeto, Informa, ademas, la distancia total de la ruta calculada.\n" << endl;
		}
		else if (comando == "ruta corta centro") {
			cout << "Identifica los indices de los vertices que conforman la ruta mas corta entre el vertice\ndado y el centro del objeto nombre_objeto \n . "<< endl;
		}
	 else if (comando == "salir") {
		cout << "Salir: Sale del programa. Usa el comando 'salir'." << endl;}
	 else {
		cout << "Comando no reconocido. Usa 'ayuda' para ver la lista de comandos disponibles." << endl;
	}
}

void mostrarAyuda() {
	cout << "Lista de comandos disponibles:" << endl;
	cout << "  1. Cargar nombre archivo - Carga un objeto 3D desde un archivo." << endl;
	cout << "  2. Listado - Muestra una lista de los objetos 3D en memoria." << endl;
	cout << "  3. Generar envolvente de un objeto - Genera una caja envolvente para un objeto 3D especC-fico." << endl;
	cout << "  4. Generar envolvente global - Genera una caja envolvente para todos los objetos en memoria." << endl;
	cout << "  5. Descargar nombre objeto - Elimina un objeto 3D de la memoria." << endl;
	cout << "  6. Guardar objeto - Guarda un objeto 3D en un archivo." << endl;
	cout << "  7. Salir - Sale del programa." << endl;
	cout << "  8. Ayuda - Muestra esta ayuda." << endl;
	cout << "  9. Vertice mas cercano a un punto (por objeto)." << endl;
	cout << "  10. Vertice mas cercano a un punto (todos los objetos)." << endl;
	cout << "  11. Vertices mas cercanos a caja envolvente." << endl;
	cout << "  12. Vertice ruta corta." << endl;
	cout << "  13. Vertices ruta corta centro." << endl;
}


// Algoritmo de Dijkstra para calcular rutas más cortas
vector<int> dijkstra(const vector<vector<float>>& distancias, int inicio, int fin, float& longitud_total) {
	int n = distancias.size();

	// Vector para almacenar la distancia mínima desde el nodo inicial a cada nodo
	vector<float> distancia(n, numeric_limits<float>::max());
	// Vector para reconstruir la ruta, almacena el nodo anterior en la ruta más corta
	vector<int> previo(n, -1);
	// Vector para marcar los nodos ya visitados
	vector<bool> visitado(n, false);
	
	// La distancia al nodo inicial es 0
	distancia[inicio] = 0;

	// Cola de prioridad para procesar los nodos por distancia mínima
	priority_queue<pair<float, int>, vector<pair<float, int>>, greater<pair<float, int>>> pq;
	pq.push({0, inicio});

	while (!pq.empty()) {
		// Extraer el nodo con la menor distancia acumulada
		int u = pq.top().second;
		pq.pop();
		if (visitado[u]) continue;
		visitado[u] = true;

		// Explorar los vecinos del nodo actual
		for (int v = 0; v < n; v++) {
			// Si hay una arista válida y el nodo vecino no ha sido visitado
			if (distancias[u][v] > 0 && !visitado[v]) {
				// Calcular la nueva distancia potencial
				float nueva_distancia = distancia[u] + distancias[u][v];
				
				// Si se encuentra una distancia más corta, se actualiza
				if (nueva_distancia < distancia[v]) {
					distancia[v] = nueva_distancia;
					previo[v] = u;
					// Añadir el vecino a la cola de prioridad con su nueva distancia
					pq.push({distancia[v], v});
				}
			}
		}
	}

	// Reconstruir la ruta desde el nodo final al inicial utilizando el vector `previo`
	vector<int> ruta;
	for (int v = fin; v != -1; v = previo[v])
		ruta.push_back(v);
	reverse(ruta.begin(), ruta.end()); // Invertir la ruta para obtener el orden correcto

	// Asignar la longitud total de la ruta más corta si existe
	if (distancia[fin] != numeric_limits<float>::max()) {
		longitud_total = distancia[fin];
	} else {
		longitud_total = -1;
	}
	return ruta; // Devuelve la ruta más corta encontrada
}

// Función para calcular la matriz de distancias entre los vértices del objeto
vector<vector<float>> calcularMatrizDistancias(const Objeto3D& objeto) {
	int n = objeto.vertices.size(); // Número de vértices en el objeto
	// Crear una matriz cuadrada de tamaño n x n inicializada con ceros
	vector<vector<float>> distancias(n, vector<float>(n, 0));
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			if (i != j) { // Evitar calcular la distancia entre un vértice y él mismo
				// Calcular la distancia euclidiana entre los vértices i y j
				distancias[i][j] = calcularDistanciaEuclidiana(objeto.vertices[i], objeto.vertices[j]);
			}
		}
	}
	return distancias; // Devolver la matriz de distancias
}

// Función para calcular la ruta más corta entre dos vértices del objeto
void ruta_corta(int i1, int i2, const string& nombre_objeto) {
	// Buscar el objeto en memoria por su nombre
	auto it = buscarObjeto(nombre_objeto);
	if (it == objetosEnMemoria.end()) {
		// Si el objeto no está en memoria, mostrar un mensaje de error
		cout << "El objeto " << nombre_objeto << " no ha sido cargado en memoria." << endl;
		return;
	}

	// Verificar si los índices de los vértices son iguales
	if (i1 == i2) {
		cout << "Los indices de los vertices dados son iguales." << endl;
		return;
	}

	// Verificar si los índices están dentro del rango válido
	if (i1 < 0 || i1 >= it->vertices.size() || i2 < 0 || i2 >= it->vertices.size()) {
		cout << "Algunos de los indices de vertices estan fuera de rango para el objeto " << nombre_objeto << "." << endl;
		return;
	}

	// Calcular la matriz de distancias entre todos los vértices del objeto
	vector<vector<float>> distancias = calcularMatrizDistancias(*it);

	// Variable para almacenar la longitud total de la ruta más corta
	float longitud_total;
	// Calcular la ruta más corta entre los vértices i1 e i2
	vector<int> ruta = dijkstra(distancias, i1, i2, longitud_total);

	// Si se encuentra una ruta válida, mostrar los vértices en la ruta y la longitud total
	if (longitud_total != -1) {
		cout << "La ruta mas corta que conecta los vertices " << i1 << " y " << i2 << " del objeto " << nombre_objeto << " pasa por: ";
		for (int vertice : ruta) {
			cout << vertice << " "; // Mostrar cada vértice en la ruta
		}
		cout << "; con una longitud de " << longitud_total << "." << endl;
	} else {
		cout << "No existe ruta entre los vertices dados." << endl;
	}
}

// Función para calcular el centroide de un objeto 3D
vector<float> calcularCentroide(const Objeto3D& objeto) {
	int n = objeto.vertices.size(); 
	vector<float> centroide(3, 0);
	for (const auto& vertice : objeto.vertices) {
		// Sumar las coordenadas de todos los vértices
		centroide[0] += vertice[0];
		centroide[1] += vertice[1];
		centroide[2] += vertice[2];
	}
	// Dividir entre el número total de vértices para calcular el promedio
	centroide[0] /= n;
	centroide[1] /= n;
	centroide[2] /= n;
	return centroide; 
}

// Función para calcular la ruta más corta desde un vértice al centro del objeto
void ruta_corta_centro(int i1, const string& nombre_objeto) {
	// Buscar el objeto en memoria por su nombre
	auto it = buscarObjeto(nombre_objeto);
	if (it == objetosEnMemoria.end()) {
		cout << "El objeto " << nombre_objeto << " no ha sido cargado en memoria." << endl;
		return;
	}

	// Verificar si el índice del vértice está dentro del rango válido
	if (i1 < 0 || i1 >= it->vertices.size()) {
		cout << "El indice de vertice esta fuera de rango para el objeto " << nombre_objeto << "." << endl;
		return;
	}

	// Calcular el centroide del objeto
	vector<float> centroide = calcularCentroide(*it);

	// Determinar el vértice más cercano al centroide
	int vertice_cercano = 0;
	float distancia_minima = calcularDistanciaEuclidiana(centroide, it->vertices[0]); 
	for (int i = 1; i < it->vertices.size(); i++) {
		float distancia = calcularDistanciaEuclidiana(centroide, it->vertices[i]);
		if (distancia < distancia_minima) { 
			distancia_minima = distancia;
			vertice_cercano = i;
		}
	}

	// Calcular la matriz de distancias entre los vértices del objeto
	vector<vector<float>> distancias = calcularMatrizDistancias(*it);

	// Calcular la ruta más corta desde el vértice inicial al vértice más cercano al centroide
	float longitud_total;
	vector<int> ruta = dijkstra(distancias, i1, vertice_cercano, longitud_total);

	// Verificar si se encontró una ruta válida
	if (longitud_total != -1) {
		cout << "La ruta mas corta que conecta el vertice " << i1 << " con el centro del objeto " << nombre_objeto << ", ubicado en (";
		cout << centroide[0] << ", " << centroide[1] << ", " << centroide[2] << "), pasa por: ";
		for (int vertice : ruta) {
			cout << vertice << " "; 
		}
		cout << "; con una longitud de " << longitud_total + distancia_minima << "." << endl;
	} else {
		cout << "No existe ruta entre el vertice y el centro." << endl;
	}
}
/*
        else if (comando == "ruta_corta") {
            int i1, i2;
            string nombre_objeto;
            iss >> i1 >> i2 >> nombre_objeto;
            ruta_corta(i1, i2, nombre_objeto);
        }
        else if (comando == "ruta_corta_centro") {
            int i1;
            string nombre_objeto;
            iss >> i1 >> nombre_objeto;
            ruta_corta_centro(i1, nombre_objeto);
        }
        */