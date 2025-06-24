#include <stdlib.h>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>
#include <iostream>
#include <fstream>
#include <sstream> 
#include <string>
#include <list>
#include <vector>
#include <regex>
#include <cmath>
#include "tinyxml2.h"
//namespaces
using namespace std;
using namespace tinyxml2;

//PI
#define PI 2 * acos(0.0)

const char* path_xml = "..\\..\\xml\\solar_system.xml";

#pragma region Variaveis Globais
//Paths
std::string path_3d = "..\\..\\3d\\";

//Variáveis comandos
int trigger_on_off = 0;
int mouse_trigger_on_off = 0;
int catmull_orbit = 1;
int camera_mode = 0;
int axis_trigger = 0;
int mode_draw = GL_LINE;
int mode_face = GL_FRONT;
float direction = 0;
float zoom = 0.985;
float movement_factor = 10;
float rotate_factor = 0.1;
float x_spherical_movement_factor = 10;
float y_spherical_movement_factor = 5;
float first_person_movement_factor = 4;
float mouseX = 0, mouseY = 0;
float boost_factor_translation = 1;
float boost_factor_rotation = 1;

//Variveis globais do xml
//Variaveis da window
int width = 512, height = 512;

//Variaveis da camera
//FIXED PRIMEIRA POSIÇÂO DE SEMPRE  (isto existe por razões de optimização)
float FposX = 10, FposY = 10, FposZ = 10;
float FlookX = 0, FlookY = 0, FlookZ = 0;
float FupX = 0, FupY = 1, FupZ = 0;

//variable
float posX = 10, posY = 10, posZ = 10;
float lookX = 0, lookY = 0, lookZ = 0;
float upX = 0, upY = 1, upZ = 0;
float ang_beta = 0, ang_alpha = 0, radius = 0;

//Variaveis da Projeção
float fov = 60, near = 1, far = 1000;

//Variaveis Camera Reset Animation
float fractionalX;
float fractionalY;
float fractionalZ;
int speed_animation=1;
int camera_reset_animation = 0;
int frames_reset_animation = 0;

//FPS and control variables
int timebase;
float frames=0;
int catmull_delay = 0;
int nr_triangulos=0;
float framespersecond=0;

//Lista de Models/Transformations.......
struct time_var
{
	float t = 0;
	float tempo_def = 0;
	float tempo_atual = 0;
	int align = 0;
};

struct Grupos{
	int id;
	int is_POP = 0;
	int nr_transformations_novas=0;

	list<float> color = {1.0, 1.0, 1.0};

	list<GLuint> vertices;
	list<GLuint> verticesCount;

	list<string> list_models;
	list<string> list_transformations;
};

struct Translation
{
	int id;
	time_var tempo;
	vector<float> points_tranlation;
};

list<Translation> list_translations;
list<Grupos> list_grupos;

#pragma endregion

#pragma region Funcoes Auxiliares
/*
* Função para mostrar na consola toda os nossos dados (USADO PARA DEBUGGING SÓ)
*/
void struct_viewer() {
	for (auto it = list_grupos.begin(); it != list_grupos.end(); it++) {
		cout << "Grupo " << it->id << ":" << endl;
		cout << "nr_transformations_novas: " << it->nr_transformations_novas << endl;
		cout << "color: ";
		for (const auto& c : it->color) {
			cout << c << " ";
		}
		cout << endl;

		cout << "vertices: ";
		for (const auto& v : it->vertices) {
			cout << v << " ";
		}
		cout << endl;

		cout << "verticesCount: ";
		for (const auto& vc : it->verticesCount) {
			cout << vc << " ";
		}
		cout << endl;

		cout << "list_models:" << endl;
		for (const auto& model : it->list_models) {
			cout << "    - " << model << endl;
		}

		cout << "  - list_transformations:" << endl;
		for (const auto& transformation : it->list_transformations) {
			cout << "    - " << transformation << endl;
		}

		cout << endl;
	}
}

/*
* Função para mostrar o que está dentro da lista de vetores das translações com pontos
*/
void translation_viewer()
{
	// Percorre cada elemento da lista de Translations
	for (auto it = list_translations.begin(); it != list_translations.end(); ++it)
	{
		cout << "ID: " << it->id << endl;
		cout << "Time: " << it->tempo.t << endl;
		cout << "Def: " << it->tempo.tempo_def << endl;
		cout << "Novo Tempo: " << it->tempo.tempo_def << endl;
		cout << "Align: " << it->tempo.align << endl;
		cout << "Points: ";

		// Percorre cada ponto da Translation atual
		int point_count = 0;
		for (auto point_it = it->points_tranlation.begin(); point_it != it->points_tranlation.end(); ++point_it)
		{
			cout << *point_it << " ";
			point_count++;

			if (point_count % 3 == 0)
			{
				cout << endl;
			}
		}

		cout << endl;
	}
}


/*
* Função de concat para cada char da string
* Esta função existe para corrigir um bug que tinhamos com a função "getline"
*/
std::string model_extrator(list<string> model, int n)
{
	int i = 0;
	for (auto it = model.begin(); it != model.end(); it++) {
		if (i == n) {
			return *it;
		}
	i++;
	}
	return "0";
}

/*
* Função que mostra na consola todos os comandos possíveis
*/
void keyMapInfo(void)
{
	cout << "========================================================Commands========================================================" << endl;
	cout << endl;
	cout << "P -> Lock/Unlock moviment" << endl;
	cout << "R -> Reset Camera and Lock" << endl;
	cout << "+ -> Zoom In                                            Requires Unlocked Moviment and Spherical View" << endl;
	cout << "- -> Zoom Out                                           Requires Unlocked Moviment and Spherical View" << endl;
	cout << "W -> Move Forward                                       Requires Unlocked Moviment and First Person View" << endl;
	cout << "S -> Move Backward                                      Requires Unlocked Moviment and First Person View" << endl;
	cout << "A -> Move Left                                          Requires Unlocked Moviment and First Person View" << endl;
	cout << "D -> Move Right                                         Requires Unlocked Moviment and First Person View" << endl;
	cout << "I -> Move Up                                            Requires Unlocked Moviment and First Person View" << endl;
	cout << "K -> Move Down                                          Requires Unlocked Moviment and First Person View" << endl;
	cout << "M -> Switch View's Type                                 Requires Unlocked Moviment" << endl;
	cout << "U -> 2x Translation Speed                               Requires Unlocked Moviment" << endl;
	cout << "J -> 2/ Translation Speed                               Requires Unlocked Moviment" << endl;
	cout << "Y -> 2x Rotation Speed                                  Requires Unlocked Moviment" << endl;
	cout << "H -> 2/ Rotation Speed                                  Requires Unlocked Moviment" << endl;
	cout << "Z -> Toggle Axis On/OFF                                 Requires Unlocked Moviment" << endl;
	cout << "C -> Toggle Catmull Lines On/OFF                        Requires Unlocked Moviment" << endl;
	cout << endl;
	cout << "Left-Mouse-Button -> Lock/Unlock Spherical Moviment     Requires Unlocked Moviment and Spherical View" << endl;
	cout << "Left-Mouse-Motion -> Move the camera                    Requires Unlocked Moviment and Spherical View/First Person View" << endl;
	cout << endl;
	cout << "8 -> Switch Mode Draw                                   Requires Unlocked Moviment" << endl;
	cout << "9 -> Switch Mode Face                                   Requires Unlocked Moviment" << endl;
	cout << endl;
	cout << "========================================================================================================================" << endl;
}

/*
* Função que calcula os fps
*/
void fps_calculator(void)
{
	frames++;
	float time = glutGet(GLUT_ELAPSED_TIME);
	if ( (time - timebase) > 1000) {
		framespersecond = frames * 1000.0 / (time - timebase);
		timebase = time;
		frames = 0;
		string fps = "CG_TP16_phase3 - FPS: " + to_string(framespersecond) +" || " + to_string(nr_triangulos) +" Triangulos";
		const char* window_title = fps.c_str();
		glutSetWindowTitle(window_title);
	}
}

/*
* Função para contruir a Matriz Raiz
*/
void buildRotMatrix(float* x, float* y, float* z, float* m) {

	m[0] = x[0]; m[1] = x[1]; m[2] = x[2]; m[3] = 0;
	m[4] = y[0]; m[5] = y[1]; m[6] = y[2]; m[7] = 0;
	m[8] = z[0]; m[9] = z[1]; m[10] = z[2]; m[11] = 0;
	m[12] = 0; m[13] = 0; m[14] = 0; m[15] = 1;
}

/*
* Função cross (para multiplicar 2 vetores)
*/
void cross(float* a, float* b, float* res) {

	res[0] = a[1] * b[2] - a[2] * b[1];
	res[1] = a[2] * b[0] - a[0] * b[2];
	res[2] = a[0] * b[1] - a[1] * b[0];
}

/*
* Função para normalizar
*/
void normalize(float* a) {

	float l = sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
	a[0] = a[0] / l;
	a[1] = a[1] / l;
	a[2] = a[2] / l;
}

/*
* Função Tamanho de um vetor
*/
float length(float* v) {

	float res = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
	return res;

}

/*
* Função multiplicar um vetor e uma matriz e dar como resultado um vetor
*/
void multMatrixVector(float* m, float* v, float* res) {

	for (int j = 0; j < 4; ++j) {
		res[j] = 0;
		for (int k = 0; k < 4; ++k) {
			res[j] += v[k] * m[j * 4 + k];
		}
	}

}
#pragma endregion

#pragma region Camera
/*
* Função para ler a posição atual da camera nas coordenadas esfericas, formulas pdf(3, page3)
*/
void loadCamera()
{	
	if (!camera_mode)
	{
		radius = sqrt((posX * posX) + (posY * posY) + (posZ * posZ));
		posX = radius * cos(ang_beta) * sin(ang_alpha);
		posZ = radius * cos(ang_beta) * cos(ang_alpha);
		posY = radius * sin(ang_beta);
	}
	if (camera_mode)
	{
		lookX = posX + cos(ang_beta) * sin(ang_alpha);
		lookZ = posZ + cos(ang_beta) * cos(ang_alpha);
		lookY = posY + sin(ang_beta);
	}
}

/*
* Função usada para inverter os angulos sempre que trocamos o modo da camera
*/
void reloadAng()
{
	ang_alpha = -ang_alpha;
	ang_beta = -ang_beta;
}

/*
* Função que dá o primeiro setup de todos da camera
*/
void setupCamera()
{
	posX = FposX;
	posY = FposY;
	posZ = FposZ;

	lookX = FlookX;
	lookY = FlookY;
	lookZ = FlookZ;

	upX = FupX;
	upY = FupY;
	upZ = FupZ;

	radius = sqrt((posX * posX) + (posY * posY) + (posZ * posZ));
	ang_beta = asin(posY / radius);
	ang_alpha = asin(posX / (radius * cos(ang_beta)));
}

/*
* Função responsavel por a camera na primeira posição registada (com animação)
*/
void resetCamera(void)
{	
	if (camera_reset_animation)
	{
		frames_reset_animation++;
		if (frames_reset_animation == 1)
		{
			//Com este calculo garantimos sempre que é praticamente 1s de animação
			speed_animation = floor(framespersecond);

			fractionalX = FposX - posX;
			fractionalY = FposY - posY;
			fractionalZ = FposZ - posZ;
		}
		int frames_def = speed_animation;

		// Fração = (Ponto Que quero ir - onde estou) / 24
		posX += fractionalX/ frames_def;
		posY += fractionalY/ frames_def;
		posZ += fractionalZ/ frames_def;

		lookX = FlookX;
		lookY = FlookY;
		lookZ = FlookZ;

		upX = FupX;
		upY = FupY;
		upZ = FupZ;

		//Bloquear qualquer tipo de movimento constantemente
		camera_mode = 0;
		trigger_on_off = 0;
		mouse_trigger_on_off = 0;

		radius = sqrt((posX * posX) + (posY * posY) + (posZ * posZ));
		ang_beta = asin(posY / radius);
		ang_alpha = asin(posX / (radius * cos(ang_beta)));

		if (frames_reset_animation == frames_def)
		{
			frames_reset_animation = 0;
			camera_reset_animation = 0;
		}
	}
}

/*
* Função responsavel puxar a camera de volta à esfera principal
*/
void fpsToExplorerCamera()
{
	radius = sqrt((FposX * FposX) + (FposY * FposY) + (FposZ * FposZ));

	ang_beta = asin(posY / radius);
	ang_alpha = asin(posX / (radius * cos(ang_beta)));

	posX = radius * cos(ang_beta) * sin(ang_alpha);
	posZ = radius * cos(ang_beta) * cos(ang_alpha);
	posY = radius * sin(ang_beta);

	lookX = FlookX;
	lookY = FlookY;
	lookZ = FlookZ;

	upX = FupX;
	upY = FupY;
	upZ = FupZ;

	camera_mode = 0;
	trigger_on_off = 0;
	mouse_trigger_on_off = 0;
}
/*
* Função para calcular o movimento da camera  em fps
* Seguimos a lógica do exercicio 3 da ficha de consolidação 2 das transformações
*/
void fpsCameraCalculator(int dir)
{
	//Calcular a direção
	float direction_x = lookX - posX;
	float direction_y = lookY - posY;
	float direction_z = lookZ - posZ;
	float direction_normalized = sqrt((direction_x * direction_x) + (direction_y * direction_y) + (direction_z * direction_z));

	//Calcular o vetor unitário de z = -( d / |d|)
	float k_x = direction_x / direction_normalized;
	float k_y = direction_y / direction_normalized;
	float k_z = direction_z / direction_normalized;

	//Calcular o vetor unitário de Y = Up / |up|
	float up_normalized = sqrt((upX * upX) + (upY * upY) + (upZ * upZ));
	float j_x = upX/up_normalized;
	float j_y = upY/ up_normalized;
	float j_z = upZ/ up_normalized;

	//Calcular o vetor unitário de x = (vetorZ * vetorY) / |vetorZ * vetorY|
	//Calculos auxiliares
	float aux_x=(k_y*j_z) -(k_z*j_y);
	float aux_y=(k_x*j_z)-(k_z*j_x);
	float aux_z=(k_x*j_y) - (k_y*j_x);
	float aux_normalized= sqrt((aux_x * aux_x) + (aux_y * aux_y) + (aux_z * aux_z));

	//Calculo do vetor unitário
	float i_x = aux_x / aux_normalized;
	float i_y = aux_y / aux_normalized;
	float i_z = aux_z / aux_normalized;

	//Direções
	//Para frente "W"
	if (dir == 1)
	{
		posX += k_x * movement_factor;
		posY += k_y * movement_factor;
		posZ += k_z * movement_factor;
		lookX += k_x * movement_factor;
		lookY += k_y * movement_factor;
		lookZ += k_z * movement_factor;
	}
	//Para baixo "S"
	else if (dir == 2)
	{
		posX -= k_x * movement_factor;
		posY -= k_y * movement_factor;
		posZ -= k_z * movement_factor;
		lookX -= k_x * movement_factor;
		lookY -= k_y * movement_factor;
		lookZ -= k_z * movement_factor;
	}
	//Para esquerda "A"
	else if (dir == 3)
	{
		posX -= i_x * movement_factor;
		posY -= i_y * movement_factor;
		posZ -= i_z * movement_factor;
		lookX -= i_x * movement_factor;
		lookY -= i_y * movement_factor;
		lookZ -= i_z * movement_factor;
	}
	//Para direita "D"
	else if (dir == 4)
	{
		posX += i_x * movement_factor;
		posY += i_y * movement_factor;
		posZ += i_z * movement_factor;
		lookX += i_x * movement_factor;
		lookY += i_y * movement_factor;
		lookZ += i_z * movement_factor;
		
	}
	//Para cima "I"
	else if (dir == 5)
	{
		posX += j_x * movement_factor;
		posY += j_y * movement_factor;
		posZ += j_z * movement_factor;
		lookX += j_x * movement_factor;
		lookY += j_y * movement_factor;
		lookZ += j_z * movement_factor;

	}
	//Para baixo "K"
	else if (dir == 6)
	{
		posX -= j_x * movement_factor;
		posY -= j_y * movement_factor;
		posZ -= j_z * movement_factor;
		lookX -= j_x * movement_factor;
		lookY -= j_y * movement_factor;
		lookZ -= j_z * movement_factor;

	}
}
#pragma endregion

#pragma region Catmull-Rom
/*
* Obter toda os dados necessarios para a translação
*/
vector <float>translation_data(int id, float* time, float* t_def, int *algn, float *novo_temp_atual)
{
	for (auto it = list_translations.begin(); it != list_translations.end(); it++) {
		{
			if (it->id == id)
			{
				*time = it->tempo.t;
				*t_def = it->tempo.tempo_def;
				*algn = it->tempo.align;
				*novo_temp_atual = it->tempo.tempo_atual;
				return it->points_tranlation;
			}
		}
	}
}

void updateTime(int id, float tempo, float novo_temp)
{
	for (auto it = list_translations.begin(); it != list_translations.end(); it++) {
		{
			if (it->id == id)
			{
				it->tempo.t = tempo;
				it->tempo.tempo_atual = novo_temp;
			}
		}
	}
}

void getCatmullRomPoint(float t, float* p0, float* p1, float* p2, float* p3, float* pos, float* deriv) {

	// catmull-rom matrix
	float m[4][4] = { {-0.5f,  1.5f, -1.5f,  0.5f},
						{ 1.0f, -2.5f,  2.0f, -0.5f},
						{-0.5f,  0.0f,  0.5f,  0.0f},
						{ 0.0f,  1.0f,  0.0f,  0.0f} };

	for (int i = 0; i < 3; i++) {
		float p[4] = { p0[i], p1[i], p2[i], p3[i] };
		float a[4];


		// Compute A = M * P
		multMatrixVector(&m[0][0], p, a);
		// Compute pos = T * A
		pos[i] = powf(t, 3.0) * a[0] + powf(t, 2.0) * a[1] + t * a[2] + a[3];

		// compute deriv = T' * A
		deriv[i] = 3 * powf(t, 2.0) * a[0] + 2 * t * a[1] + a[2];
	}
}

void getGlobalCatmullRomPoint(float gt, float* pos, float* deriv, int id , vector<float> points) {	
	float t_aux, t_def_aux;
	int align_aux;
	int point_count = (float)points.size()/3;
	float t = gt * point_count; // this is the real global t
	int index = floor(t);  // which segment
	t = t - index; // where within  the segment

	// indices store the points
	int indices[4];
	indices[0] = ((index + point_count - 1) % point_count);
	indices[1] = ((indices[0] + 1) % point_count);
	indices[2] = ((indices[1] + 1) % point_count);
	indices[3] = ((indices[2] + 1) % point_count);

	float p0[3];
	float p1[3];
	float p2[3];
	float p3[3];

	p0[0] = points[(indices[0] * 3) + 0];  //x
	p0[1] = points[(indices[0] * 3) + 1];  //y
	p0[2] = points[(indices[0] * 3) + 2];  //z

	p1[0] = points[(indices[1] * 3) + 0];  //x
	p1[1] = points[(indices[1] * 3) + 1];  //y
	p1[2] = points[(indices[1] * 3) + 2];  //z

	p2[0] = points[(indices[2] * 3) + 0];  //x
	p2[1] = points[(indices[2] * 3) + 1];  //y
	p2[2] = points[(indices[2] * 3) + 2];  //z

	p3[0] = points[(indices[3] * 3) + 0];  //x
	p3[1] = points[(indices[3] * 3) + 1];  //y
	p3[2] = points[(indices[3] * 3) + 2];  //z
	
	getCatmullRomPoint(t, p0, p1, p2, p3, pos, deriv);
}

void renderCatmullRomCurve(int id, vector<float> points) {

	float pos[3];
	float deriv[3];

	// draw curve using line segments with GL_LINE_LOOP
	if (catmull_orbit)
	{
		glBegin(GL_LINE_LOOP);
		for (float gt = 0; gt < 1; gt += 0.01) {
			glColor3f(1, 1, 1);
			getGlobalCatmullRomPoint(gt, pos, deriv, id, points);
			glVertex3f(pos[0], pos[1], pos[2]);

		}
		glEnd();
	}
}

void CatmullTransformation(int id)
{
	float t_aux, t_def_aux, tempo_atual;
	int align_aux;
	vector<float>points = translation_data(id, &t_aux, &t_def_aux, &align_aux, &tempo_atual);
	float t = t_aux;

	renderCatmullRomCurve(id, points);
	float pos[3];
	float deriv[3];
	float prev_y[3] = { 0,1,0 };

	getGlobalCatmullRomPoint(t, pos, deriv,id, points);
	glTranslatef(pos[0], pos[1], pos[2]);

	if (align_aux)
	{
		float x[3] = { deriv[0],deriv[1],deriv[2] };
		float y[3];
		float z[3];
		float m[16];
		normalize(x);
		cross(x, prev_y, z);
		normalize(z);
		cross(z, x, y);
		normalize(y);
		memcpy(prev_y, y, 3 * sizeof(float));
		buildRotMatrix(x, y, z, m);
		glMultMatrixf(m);
	}
	float novoT = glutGet(GLUT_ELAPSED_TIME);
	float deltaT = novoT - tempo_atual;

	t_def_aux = t_def_aux / boost_factor_translation;
	t += (deltaT / (t_def_aux * 1000));
	tempo_atual = novoT;

	updateTime(id, t, tempo_atual);
}

#pragma endregion

#pragma region Drawings
/*
* Função que vai ler os vertices diretamente do/s ficheiro/s e gerar o/s model/s e as suas respetivas transformações
*/
void drawModel(void)
{
	for (auto it = list_grupos.begin(); it != list_grupos.end(); it++)
	{
		for (auto model = it->list_models.begin(); model != it->list_models.end(); model++)
		{
			glPushMatrix();
			//Transformações
			for (auto trans = it->list_transformations.begin(); trans != it->list_transformations.end(); trans++)
			{
				std::string transform_string = *trans;
				istringstream iss(transform_string);
				string opcao;
				if (iss >> opcao)
				{
					float angT, xT, yT, zT, timer;
					if (opcao == "t1") {
						istringstream iss(transform_string);
						if (iss >> opcao >> xT >> yT >> zT)
						{
							glTranslatef(xT, yT, zT);
						}
					}
					else if (opcao == "t2") {
						float translation_id;
						istringstream iss(transform_string);
						if (iss >> opcao >> translation_id)
						{
								CatmullTransformation(translation_id);
						}
						
					}
					else if (opcao == "s") {
						istringstream iss(transform_string);
						if (iss >> opcao >> xT >> yT >> zT)
						{
							glScalef(xT, yT, zT);
						}
					}
					else if (opcao == "r1") {
						istringstream iss(transform_string);
						if (iss >> opcao >>angT>> xT >> yT >> zT)
						{
							glRotatef(angT,xT, yT, zT);
						}
					}
					else if (opcao == "r2")
					{
						istringstream iss(transform_string);
						if (iss >> opcao >> timer >> xT >> yT >> zT)
						{
							timer = timer / boost_factor_rotation;
							float angT = ((glutGet(GLUT_ELAPSED_TIME) * 360) / (timer*1000));
							glRotatef(angT, xT, yT, zT);
						}
					}
				}
			}
			//DESENHAR O OBJETO
			auto vbuffercount = it->verticesCount.begin();
			for (auto vbuffer = it->vertices.begin(); vbuffer != it->vertices.end(); vbuffer++,vbuffercount++)
			{
					//desenhar com a cor selecionada pelo XML
					int counter = 1;
					float c1=0, c2=0, c3=0;
					for (auto c = it->color.begin(); c != it->color.end(); c++,counter++)
					{
						if (counter == 1)
						{
							c1 = *c;
						}
						else if(counter==2)
						{
							c2 = *c;
						}
						else
						{
							c3 = *c;
						}
					}
					glColor3f(c1, c2, c3);
					glBindBuffer(GL_ARRAY_BUFFER, *vbuffer);
					glVertexPointer(3, GL_FLOAT, 0, 0);
					glDrawArrays(GL_TRIANGLES, 0, *vbuffercount);
			}
			glPopMatrix();
		}
	}
}

/*
* Função que vai gerar os axis
*/
void drawAxis()
{
	if (axis_trigger)
	{
		glBegin(GL_LINES);

		// X axis in red
		glColor3f(1.0f, 0.0f, 0.0f);
		glVertex3f(-100.0f, 0.0f, 0.0f);
		glVertex3f(100.0f, 0.0f, 0.0f);
		// Y Axis in Green
		glColor3f(0.0f, 1.0f, 0.0f);
		glVertex3f(0.0f, -100.0f, 0.0f);
		glVertex3f(0.0f, 100.0f, 0.0f);
		// Z Axis in Blue
		glColor3f(0.0f, 0.0f, 1.0f);
		glVertex3f(0.0f, 0.0f, -100.0f);
		glVertex3f(0.0f, 0.0f, 100.0f);
		glEnd();
	}
}

#pragma endregion

#pragma region DefaultFunctions Scenes
void changeSize(int w, int h)
{
	// Prevent a divide by zero, when window is too short
	// (you can�t make a window with zero width).
	if (h == 0)
		h = 1;
	// compute window's aspect ratio
	float ratio = w * 1.0f / h;
	// Set the projection matrix as current
	glMatrixMode(GL_PROJECTION);
	// Load the identity matrix
	glLoadIdentity();
	// Set the viewport to be the entire window
	glViewport(0, 0, w, h);
	// Set the perspective
	gluPerspective(fov, ratio, near, far);
	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}


void renderScene(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Camera
	glLoadIdentity();
	gluLookAt(posX, posY, posZ,
		lookX, lookY, lookZ,
		upX, upY, upZ);

	// Modes
	glPolygonMode(mode_face, mode_draw);
	drawAxis();

	// FPS
	fps_calculator();

	//Camera Reset Animation
	resetCamera();

	// Drawings
	drawModel();

	glutSwapBuffers();
}

#pragma endregion

#pragma region tinyxml2 (PARSER)

/*
* Função básica para saber se existe o elemento ou nao numa lista de elementos (tinyxml2)
*/
int isVisited(const list<XMLElement*>& elements, const XMLElement* element) {
	for (auto it = elements.begin(); it != elements.end(); ++it) {
		if (*it == element) {
			return 1;
		}
	}
	return 0;
}

/*
* Algoritmo para extrair o xml para as variaveis globais do codigo
*/
void loadXML(void)
{
	list<XMLElement*> elemVisitados;
	XMLDocument xmlDoc;
	xmlDoc.LoadFile(path_xml);
	XMLNode* pWorld = xmlDoc.FirstChild();

	if (pWorld)
	{
		//Tratar da Window
		XMLElement* pWindow = pWorld->FirstChildElement("window");
		if (pWindow)
		{
			pWindow->QueryIntAttribute("width", &width);
			pWindow->QueryIntAttribute("height", &height);
		}

		//Tratar da Camera
		XMLElement* pCamera = pWorld->FirstChildElement("camera");
		if (pCamera)
		{
			//Tratar da Posição
			XMLElement* pPosition = pCamera->FirstChildElement("position");
			if (pPosition)
			{
				pPosition->QueryFloatAttribute("x", &FposX);
				pPosition->QueryFloatAttribute("y", &FposY);
				pPosition->QueryFloatAttribute("z", &FposZ);
			}

			//Tratar do lookAt
			XMLElement* pLookAt = pCamera->FirstChildElement("lookAt");
			if (pLookAt)
			{
				pLookAt->QueryFloatAttribute("x", &FlookX);
				pLookAt->QueryFloatAttribute("y", &FlookY);
				pLookAt->QueryFloatAttribute("z", &FlookZ);
			}

			//Tratar do pUp
			XMLElement* pUp = pCamera->FirstChildElement("up");
			if (pUp)
			{
				pUp->QueryFloatAttribute("x", &FupX);
				pUp->QueryFloatAttribute("y", &FupY);
				pUp->QueryFloatAttribute("z", &FupZ);
			}

			//Tratar da projection
			XMLElement* pProjection = pCamera->FirstChildElement("projection");
			if (pProjection)
			{
				pProjection->QueryFloatAttribute("fov", &fov);
				pProjection->QueryFloatAttribute("near", &near);
				pProjection->QueryFloatAttribute("far", &far);
			}

			//Tratar os Modos
			XMLElement* pMode = pCamera->FirstChildElement("mode");
			if (pMode)
			{
				float readMode;
				pMode->QueryFloatAttribute("draw", &readMode);
				if (readMode == 1.0)
				{
					mode_draw = GL_FILL;
				}
				else if (readMode == 0)
				{
					mode_draw = GL_LINE;
				}
			}

			//Receber do utilizador se as linhas do catmull sao visiveis
			XMLElement* pCatmull = pCamera->FirstChildElement("catmull");
			if (pCatmull)
			{
				int readOrbit;
				pCatmull->QueryIntAttribute("orbit", &readOrbit);
				catmull_orbit = readOrbit;
			}
		}
		setupCamera();

		//Tratar dos grupos
		XMLElement* pGroup = pWorld->FirstChildElement("group");
		list<string> transformations_buffer;
		int new_id = 0;
		int translation_id = 0;
		while (pGroup)
		{
			if (!isVisited(elemVisitados, pGroup))
			{
				new_id++;
				Grupos temp;
				list_grupos.push_front(temp);
				list_grupos.begin()->id = new_id;
				//Tratar as transformações
				XMLElement* pTransformations = pGroup->FirstChildElement("transform");
				if (pTransformations)
				{
					int nr_tranformacoes = 0;
					//Tratar cada transformação por ordem de chegada
					XMLElement* transform = pTransformations->FirstChildElement();
					XMLElement* pTranslate = pTransformations->FirstChildElement("translate");
					XMLElement* pScale = pTransformations->FirstChildElement("scale");
					XMLElement* pRotation = pTransformations->FirstChildElement("rotate");
					while (transform != nullptr)
					{
						std::stringstream transformation;
						float time = 0, aligne = 0;
						const char* auxString;
						vector<float> aux_point;
						float rotAngle = 0, rotX = 0, rotY = 0, rotZ = 1;
						float scaleX = 1, scaleY = 1, scaleZ = 1;
						float transX = 0, transY = 0, transZ = 0;
						float aux = 0;
						float time_r = 1;
						if (pTranslate == transform)
						{
							XMLElement* pTranslatePoint = pTranslate->FirstChildElement("point");
							if (pTranslatePoint)
							{
								Translation temp_trans;
								list_translations.push_front(temp_trans);
								pTranslate->QueryFloatAttribute("time", &aux);
								time = aux;
								pTranslate->QueryStringAttribute("align", &auxString);
								if (!strcmp(auxString, "true"))
								{

									aligne = 1;
								}
								else
								{
									aligne = 0;
								}
								while (pTranslatePoint)
								{
									float transX_aux = 0, transY_aux = 0, transZ_aux = 0;
									pTranslatePoint->QueryFloatAttribute("x", &aux);
									transX_aux = aux;
									pTranslatePoint->QueryFloatAttribute("y", &aux);
									transY_aux = aux;
									pTranslatePoint->QueryFloatAttribute("z", &aux);
									transZ_aux = aux;
									aux_point.push_back(transX_aux);
									aux_point.push_back(transY_aux);
									aux_point.push_back(transZ_aux);
									pTranslatePoint = pTranslatePoint->NextSiblingElement("point");
								}
								translation_id++;
								list_translations.begin()->points_tranlation = aux_point;
								list_translations.begin()->id = translation_id;
								list_translations.begin()->tempo.align = aligne;
								list_translations.begin()->tempo.tempo_def = time;
								list_translations.begin()->tempo.tempo_atual = glutGet(GLUT_ELAPSED_TIME);
								transformation << "t2" << " " << translation_id;
							}
							else
							{
								//Obter as translações do guiao 2 (sem tempo associado)
								pTranslate->QueryFloatAttribute("x", &aux);
								transX = aux;
								pTranslate->QueryFloatAttribute("y", &aux);
								transY = aux;
								pTranslate->QueryFloatAttribute("z", &aux);
								transZ = aux;
								transformation << "t1" << " " << transX << " " << transY << " " << transZ;
							}
							pTranslate = pTranslate->NextSiblingElement("translate");
						}
						//Tratar a escala
						if (pScale == transform)
						{
							pScale->QueryFloatAttribute("x", &aux);
							scaleX = aux;
							pScale->QueryFloatAttribute("y", &aux);
							scaleY = aux;
							pScale->QueryFloatAttribute("z", &aux);
							scaleZ = aux;
							transformation << "s" << " " << scaleX << " " << scaleY << " " << scaleZ;
							pScale = pScale->NextSiblingElement("scale");
						}
						//Tratar as rotações
						if (pRotation == transform)
						{
							if (!pRotation->QueryFloatAttribute("time", &aux))
							{
								pRotation->QueryFloatAttribute("time", &aux);
								time_r = aux;
								pRotation->QueryFloatAttribute("x", &aux);
								rotX = aux;
								pRotation->QueryFloatAttribute("y", &aux);
								rotY = aux;
								pRotation->QueryFloatAttribute("z", &aux);
								rotZ = aux;
								transformation << "r2" << " " << time_r << " " << rotX << " " << rotY << " " << rotZ;
							}
							else
							{
								pRotation->QueryFloatAttribute("angle", &aux);
								rotAngle = aux;
								pRotation->QueryFloatAttribute("x", &aux);
								rotX = aux;
								pRotation->QueryFloatAttribute("y", &aux);
								rotY = aux;
								pRotation->QueryFloatAttribute("z", &aux);
								rotZ = aux;
								transformation << "r1" << " " << rotAngle << " " << rotX << " " << rotY << " " << rotZ;
							}
							pRotation = pRotation->NextSiblingElement("rotate");
						}
						nr_tranformacoes++;
						transformations_buffer.push_back(transformation.str());
						transform = transform->NextSiblingElement();
					}
					//Guardar todos os dados necessarios das transformaçoes deste grupo
					list_grupos.begin()->nr_transformations_novas = nr_tranformacoes;
					list_grupos.begin()->list_transformations = transformations_buffer;
				}

				//Tratar os models  AQUI POSSO TER DIFERENTES GRUPOS
				XMLElement* pListModel = pGroup->FirstChildElement("models");
				if (pListModel)
				{
					//Tratar cada modelo dentro dos modelos
					XMLElement* model = pListModel->FirstChildElement("model");
					while (model != nullptr)
					{
						list_grupos.begin()->list_models.push_back(model->Attribute("file"));
						//para cada modelo, encontrar os primos até nao existir mais primos
						model = model->NextSiblingElement("model");
					}
				}
				//Tratar as settings de cada grupo
				XMLElement* pSettings = pGroup->FirstChildElement("settings");
				if (pSettings)
				{
					XMLElement* color = pSettings->FirstChildElement("color");
					if (color)
					{
						float color1, color2, color3;
						color->QueryFloatAttribute("c1", &color1);
						color->QueryFloatAttribute("c2", &color2);
						color->QueryFloatAttribute("c3", &color3);
						list_grupos.begin()->color = { color1,color2,color3 };
					}
				}
			}

			elemVisitados.push_back(pGroup);
			XMLElement* next_pGroup = pGroup->FirstChildElement("group");
			//Percorrer primeiro os filhos
			if (next_pGroup && !isVisited(elemVisitados, next_pGroup))
			{
				pGroup = next_pGroup;
			}
			else
			{
				// Se Nao existir filho
				// MAS PRIMEIRO:
				// Dar multiPOP das transformacoes atuais
				next_pGroup = pGroup->NextSiblingElement("group");
				if (next_pGroup && !isVisited(elemVisitados, next_pGroup))
				{
					//Processo de multiPOP das transformações acumuladas
					for (auto it = list_grupos.begin(); it != list_grupos.end(); it++)
					{
						if (it->is_POP == 0)
						{
							for (int i = 0; i < it->nr_transformations_novas; i++)
							{
								transformations_buffer.pop_back();
							}
							it->is_POP = 1;
							break;
						}
					}

					pGroup = next_pGroup;
				}
				//Se tudo estiver percorrido, voltar ao pai
				else
				{
					pGroup = pGroup->Parent()->ToElement();
					//acabar com o ciclo principal
					if (pWorld->ToElement() == pGroup && !pGroup->NextSiblingElement("group"))
					{
						pGroup = nullptr;
					}
					//Caso volte ao pai, repetir o processo do multiPOP para o grupo atual FALTA AQUI!!!!!!!!!!!
					for (auto it = list_grupos.begin(); it != list_grupos.end(); it++)
					{
						if (it->is_POP == 0)
						{
							for (int i = 0; i < it->nr_transformations_novas; i++)
							{
								transformations_buffer.pop_back();
							}
							it->is_POP = 1;
							break;
						}
					}
				}
			}
		}
	}
}

#pragma endregion

#pragma region Comandos

void keyMap(unsigned char c, int x, int y) {
	// put code to process regular keys in here
	switch (c) {
	case 'p':
		trigger_on_off = (trigger_on_off + 1) % 2;
		break;
	case 'r':
		camera_reset_animation = (camera_reset_animation + 1) % 2;
		break;
	case 'm':
		if (trigger_on_off)
		{
			camera_mode = (camera_mode + 1) % 2;
			if (!camera_mode)
			{
				fpsToExplorerCamera();
			}
			if (camera_mode)
			{
				reloadAng();
			}
		}
		break;
	case '8':
		if (trigger_on_off)
		{
			if (mode_draw == GL_LINE)
			{
				mode_draw = GL_FILL;
			}
			else
			{
				mode_draw = GL_LINE;
			}
		}
		break;
	case '9':
		if (trigger_on_off)
		{
			if (mode_face == GL_FRONT)
			{
				mode_face = GL_BACK;
			}
			else if (mode_face == GL_BACK)
			{
				mode_face = GL_FRONT_AND_BACK;
			}
			else
			{
				mode_face = GL_FRONT;
			}
		}
		break;
	case 'w':
		if (trigger_on_off && camera_mode)
		{
			fpsCameraCalculator(1);
		}
		break;
	case 's':
		if (trigger_on_off && camera_mode)
		{
			fpsCameraCalculator(2);
		}
		break;
	case 'a':
		if (trigger_on_off && camera_mode)
		{
			fpsCameraCalculator(3);
		}
		break;
	case 'd':
		if (trigger_on_off && camera_mode)
		{
			fpsCameraCalculator(4);
		}
		break;
	case 'i':
		if (trigger_on_off && camera_mode)
		{
			fpsCameraCalculator(5);
		}
		break;
	case 'k':
		if (trigger_on_off && camera_mode)
		{
			fpsCameraCalculator(6);
		}
		break;
	case '+':
		if (trigger_on_off && !camera_mode)
		{
			posX *= zoom;
			posY *= zoom;
			posZ *= zoom;
		}
		break;
	case '-':
		if (trigger_on_off && !camera_mode)
		{
			posX /= zoom;
			posY /= zoom;
			posZ /= zoom;
		}
		break;
	case 'z':
		if (trigger_on_off && !camera_mode)
		{
			axis_trigger = (axis_trigger+ 1) % 2;
		}
		break;
	case 'c':
		if (trigger_on_off)
		{
			catmull_orbit = (catmull_orbit + 1) % 2;
		}
		break;
	case 'j':
		if (trigger_on_off)
		{
			boost_factor_translation /= 5;
		}
		break;
	case 'u':
		if (trigger_on_off)
		{
			boost_factor_translation *= 2;
		}
		break;
	case 'y':
		if (trigger_on_off)
		{
			boost_factor_rotation *= 2;
		}
		break;
	case 'h':
		if (trigger_on_off)
		{
			boost_factor_rotation /= 2;
		}
		break;
	default:
		break;
	}
	loadCamera();
}

void mouseClicks(int button, int state, int x, int y)
{
	switch (button) {
	case GLUT_LEFT_BUTTON:
		if (trigger_on_off && !camera_mode)
		{
			if (state == GLUT_DOWN) {
				mouse_trigger_on_off = (mouse_trigger_on_off + 1) % 2;
			}
		}
		break;
	default:
		break;
	}
}

void mouseMotion(int x, int y)
{
	float camera_rotationX = x_spherical_movement_factor * PI / width;
	float camera_rotationY = y_spherical_movement_factor * PI / height;
	if (camera_mode)
	{
		camera_rotationX = first_person_movement_factor * PI / width;
		camera_rotationY = first_person_movement_factor * PI / height;
	}

	if ((trigger_on_off && mouse_trigger_on_off && !camera_mode) ||
		(trigger_on_off && camera_mode))
	{

		if (x > mouseX) {
			ang_alpha -= camera_rotationX;
			mouseX = x;
		}
		if (x < mouseX) {
			ang_alpha += camera_rotationX;
			mouseX = x;
		}

		//O Beta está compreendido entre -90 e 90 graus!!!
		//Caso isto aconteça, ficamos com a visão de topo ou de baixo
		if (y > mouseY && ang_beta >= -(PI / 2)) {
			ang_beta -= camera_rotationY;
			mouseY = y;
		}
		if (y < mouseY && ang_beta <= (PI / 2)) {
			ang_beta += camera_rotationY;
			mouseY = y;
		}
		mouseY = y;
		mouseX = x;
		loadCamera();
	}
	//Em Caso de FreeCamera
}

#pragma endregion

#pragma region Processar Data

/*
* Função básica usando regex para descubrir dar match ao numero de triangulos
*/
int extract_Triangles(string triangle_str)
{
	regex pattern("\\*\\*Triangulos Desenhados: (\\d+)");
	smatch matches;
	if (regex_search(triangle_str, matches, pattern)) {
		return stoi(matches[1]);
	}
}

/*
* Função que recebe os models e coloca-os no buffer (VBOs)
*/
void prepareData()
{
	std::string model_name = "";
	for (auto it = list_grupos.begin(); it != list_grupos.end(); it++)
	{
		
		int counter = 0;
		for (auto model = it->list_models.begin(); model != it->list_models.end(); model++, counter++)
		{
			vector<float> p;

			model_name = model_extrator(it->list_models, counter);
			std::string line;
			std::string path_model = path_3d + model_name;
			ifstream myfile(path_model);
			float x, y, z;
			if (myfile.is_open())
			{
				//DESENHAR O OBJETO
				while (getline(myfile, line))
				{		
	
					//Ignorar a descrição
					if (line[0] == '!' || line[0]=='*')
					{
						if (line[0] == '*')
						{
							nr_triangulos += extract_Triangles(line);
						}
					}
					else
					{
						//Para indexar cada linha
						istringstream iss(line);
						//x é o index 0, y index 1 e o z index 2
						if (iss >> x >> y >> z)
						{
							//mandar para o buffer
							p.push_back(x);
							p.push_back(y);
							p.push_back(z);
						}
					}
				}
				myfile.close();
			}
			//Criar o VBO para este model
			GLuint vertices_aux, verticeCount_aux;
			verticeCount_aux = p.size() / 3;

			//Criar o VBO
			glGenBuffers(1, &vertices_aux);

			//Mandar para a VRam
			glBindBuffer(GL_ARRAY_BUFFER, vertices_aux);
			glBufferData(GL_ARRAY_BUFFER,
				sizeof(float) * p.size(),
				p.data(),
				GL_STATIC_DRAW);

			//Guardar na struct
			it->verticesCount.push_back(verticeCount_aux);
			it->vertices.push_back(vertices_aux);

		}
	} 
}

#pragma endregion

int main(int argc, char** argv)
{
	//Comandos
	keyMapInfo();
	//load xml
	loadXML();

	// Glut init
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(150, 50);
	glutInitWindowSize(width, height);
	glutCreateWindow("CG_TP16_phase3 ");

	// Callbacks
	glutReshapeFunc(changeSize);
	glutIdleFunc(renderScene);
	glutDisplayFunc(renderScene);
	glutKeyboardFunc(keyMap);
	glutPassiveMotionFunc(mouseMotion);
	glutMouseFunc(mouseClicks);


	// Settings OpenGL
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

	timebase = glutGet(GLUT_ELAPSED_TIME);

	// Glew init
	glewInit();
	glEnableClientState(GL_VERTEX_ARRAY);

	//VBOs
	prepareData();

	// Run
	glutMainLoop();

	return 1;
}
