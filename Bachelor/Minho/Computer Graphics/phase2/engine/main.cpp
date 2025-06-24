#include <stdlib.h>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
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
#include "tinyxml2.h"
//namespaces
using namespace std;
using namespace tinyxml2;

//PI
#define PI 2 * acos(0.0)

const char* path_xml = "..\\..\\xml\\xwing.xml";

#pragma region Variaveis Globais
//Paths
std::string path_3d = "..\\..\\3d\\";

//Variáveis comandos
int trigger_on_off = 0;
int mouse_trigger_on_off = 0;
int camera_mode = 0;
int axis_trigger = 0;
int mode_draw = GL_LINE;
int mode_face = GL_FRONT;
float direction = 0;
float zoom = 0.985;
float movement_factor = 0.01;
float x_spherical_movement_factor = 40;
float y_spherical_movement_factor = 10;
float first_person_movement_factor = 40;
float mouseX = 0, mouseY = 0;

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

//Lista de Models/Transformations.......
struct Grupos{
	int id;
	int is_POP = 0;
	int nr_transformations_novas=0;
	list<float> color = {1.0, 1.0, 1.0};
	list<string> list_models;
	list<string> list_transformations;
};

list<Grupos> list_grupos;

#pragma endregion

#pragma region Funcoes Auxiliares
/*
* Função para mostrar na consola toda os nossos dados (USADO PARA DEBUGGING SÓ)
*/
void struct_viewer(void)
{
	for (auto it = list_grupos.begin(); it != list_grupos.end(); it++) {
		cout << "Grupo " << it->id << " com " << it->nr_transformations_novas << " nr de transformacoes" << endl;

		cout << "Lista de models:" << endl;
		for (auto it2 = it->list_models.begin(); it2 != it->list_models.end(); it2++) {
			cout << *it2 << std::endl;
		}

		cout << "Lista de tranformations:" << endl;
		for (auto it2 = it->list_transformations.begin(); it2 != it->list_transformations.end(); it2++) {
			cout << *it2 << endl;
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
	cout << "M -> Switch View's Type                                 Requires Unlocked Moviment" << endl;
	cout << "A -> Toggle Axis On/OFF                                 Requires Unlocked Moviment" << endl;
	cout << endl;
	cout << "Left-Mouse-Button -> Lock/Unlock Spherical Moviment     Requires Unlocked Moviment and Spherical View" << endl;
	cout << "Left-Mouse-Motion -> Move the camera                    Requires Unlocked Moviment and Spherical View/First Person View" << endl;
	cout << endl;
	cout << "8 -> Switch Mode Draw                                   Requires Unlocked Moviment" << endl;
	cout << "9 -> Switch Mode Face                                   Requires Unlocked Moviment" << endl;
	cout << endl;
	cout << "========================================================================================================================";
}

#pragma endregion

#pragma region Camera
/*
* Função para ler a posição atual da camera nas coordenadas esfericas, formulas pdf(3, page3)
*/
void loadCamera()
{
	radius = sqrt((posX * posX) + (posY * posY) + (posZ * posZ));
	if (!camera_mode)
	{
		posX = radius * cos(ang_beta) * sin(ang_alpha);
		posZ = radius * cos(ang_beta) * cos(ang_alpha);
		posY = radius * sin(ang_beta);
	}
	if (camera_mode)
	{
		lookX = radius * cos(ang_beta) * sin(ang_alpha);
		lookZ = radius * cos(ang_beta) * cos(ang_alpha);
		lookY = radius * sin(ang_beta);
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
* Função responsavil por a camera na primeira posição registada
*/
void resetCamera()
{


	posX = FposX;
	posY = FposY;
	posZ = FposZ;

	lookX = FlookX;
	lookY = FlookX;
	lookZ = FlookX;

	upX = FupX;
	upY = FupY;
	upZ = FupZ;

	radius = sqrt((posX * posX) + (posY * posY) + (posZ * posZ));
	ang_beta = asin(posY / radius);
	ang_alpha = asin(posX / (radius * cos(ang_beta)));
}
#pragma endregion

#pragma region Drawings
/*
* Função que vai ler os vertices diretamente do/s ficheiro/s e gerar o/s model/s e as suas respetivas transformações
*/
void drawModel(void)
{
	std::string model_name = "";
	for (auto it = list_grupos.begin(); it != list_grupos.end(); it++)
	{
		
		int counter = 0;
		for (auto model = it->list_models.begin(); model != it->list_models.end(); model++, counter++)
		{
			glPushMatrix();
			model_name = model_extrator(it->list_models, counter);
			std::string line;
			std::string path_model = path_3d + model_name;
			ifstream myfile(path_model);
			float x, y, z;
			if (myfile.is_open())
			{

				//Transformações
				for (auto trans = it->list_transformations.begin(); trans != it->list_transformations.end(); trans++)
				{
					std::string transform_string = *trans;
					istringstream iss(transform_string);
					string opcao;
					if (iss >> opcao)
					{
						float angT, xT, yT, zT;
						if (opcao == "t") {
							istringstream iss(transform_string);
							if (iss >> opcao >> xT >> yT >> zT)
							{
								glTranslatef(xT, yT, zT);
							}
						}
						else if (opcao == "s") {
							istringstream iss(transform_string);
							if (iss >> opcao >> xT >> yT >> zT)
							{
								glScalef(xT, yT, zT);
							}
						}
						else if (opcao == "r") {
							istringstream iss(transform_string);
							if (iss >> opcao >>angT>> xT >> yT >> zT)
							{
								glRotatef(angT,xT, yT, zT);
							}
						}
					}
				}
				//DESENHAR O OBJETO
				glBegin(GL_TRIANGLES);
				while (getline(myfile, line))
				{
					//Ignorar as caracterizações dos ficheiros 3d
					if (line[0] != '!')
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

						//Para indexar cada linha
						istringstream iss(line);
						//x é o index 0, y index 1 e o z index 2
						if (iss >> x >> y >> z)
						{
							glVertex3f(x, y, z);
						}
					}
				}
				glEnd();
				glPopMatrix();
				myfile.close();
			}
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
				//Formula no pdf 3, pag3
				radius = sqrt((FposX * FposX) + (FposY * FposY) + (FposZ * FposZ));

				//Formulas inversas no pdf 3, pag 2 e 3
				ang_beta = asin(FposY / radius);
				ang_alpha = asin(FposX / (radius * cos(ang_beta)));
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
			resetCamera();
		}

		//Tratar dos grupos
		XMLElement* pGroup = pWorld->FirstChildElement("group");
		list<string> transformations_buffer;
		int new_id=0;
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
						float rotAngle = 0, rotX = 0, rotY = 0, rotZ = 1;
						float scaleX = 1, scaleY = 1, scaleZ = 1;
						float transX = 0, transY = 0, transZ = 0;
						float aux = 0;
						
						if (pTranslate == transform)
						{
							pTranslate->QueryFloatAttribute("x", &aux);
							transX = aux;
							pTranslate->QueryFloatAttribute("y", &aux);
							transY = aux;
							pTranslate->QueryFloatAttribute("z", &aux);
							transZ = aux;
							transformation <<"t"<< " " << transX << " " << transY << " " << transZ;
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
							pRotation->QueryFloatAttribute("angle", &aux);
							rotAngle = aux;
							pRotation->QueryFloatAttribute("x", &aux);
							rotX = aux;
							pRotation->QueryFloatAttribute("y", &aux);
							rotY = aux;
							pRotation->QueryFloatAttribute("z", &aux);
							rotZ = aux;
							transformation << "r" << " " <<rotAngle << " " << rotX << " " << rotY << " " << rotZ;
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
			if (next_pGroup && !isVisited(elemVisitados,next_pGroup))
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
		resetCamera();
		camera_mode = 0;
		trigger_on_off = 0;
		mouse_trigger_on_off = 0;
		break;
	case 'm':
		if (trigger_on_off)
		{
			camera_mode = (camera_mode + 1) % 2;
			reloadAng();
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
			posX += (lookX * movement_factor);
			posY += (lookY * movement_factor);
			posZ += (lookZ * movement_factor);
		}
		break;
	case 's':
		if (trigger_on_off && camera_mode)
		{
			posX -= (lookX * movement_factor);
			posY -= (lookY * movement_factor);
			posZ -= (lookZ * movement_factor);
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
	case 'a':
		if (trigger_on_off && !camera_mode)
		{
			axis_trigger = (axis_trigger+ 1) % 2;
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
	glutCreateWindow("CG_TP16_phase2");

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

	// Run
	glutMainLoop();

	return 1;
}