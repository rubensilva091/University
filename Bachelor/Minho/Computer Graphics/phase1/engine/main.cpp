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
#include "tinyxml2.h"
//namespaces
using namespace std;
using namespace tinyxml2;

//PI
#define PI 2 * acos(0.0)

#pragma region Variaveis Globais
//Paths
std::string path_3d =  "..\\..\\3d\\";
const char* path_xml = "..\\..\\xml\\test_1_1.xml";

//Variáveis comandos
int trigger_on_off = 0;
int mouse_trigger_on_off = 0;
int camera_mode = 0;
int mode_draw = GL_LINE;
int mode_face = GL_FRONT;
float direction = 0; 
float zoom = 0.9;
float movement_factor = 0.05;
float mouseX=0, mouseY=0;

//Variveis globais do xml
//Variaveis da window
int width=800, height=800;

//Variaveis da camera
float posX=0, posY=0, posZ=0;
float lookX=0, lookY=0, lookZ=0;
float upX=0, upY=0, upZ=0;
float ang_beta = 0, ang_alpha = 0,radius=0;

//Variaveis da Projeção
float fov=45, near=1, far=1000;

//Lista de models
std::string list_models="";
int list_models_counter = 0;

#pragma endregion

/*
* Função de concat para cada char da string
* Esta função existe para corrigir um bug que tinhamos com a função "getline"
*/
std::string concatCharsUntil(std::string str1, int &i)
{
	for (i; list_models[i] != '.'; i++)
	{
		str1 += list_models[i];
	}
	str1 += list_models[i];
	i++;
	str1 += list_models[i];
	i++;
	str1 += list_models[i];
	i++;
	return str1;
}

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

#pragma endregion

/*
* Função que vai ler os vertices diretamente do/s ficheiro/s e gerar o/s model/s
*/
void drawModel(void)
{
	std::string model_name="";
	int counter = 0;
	int i = 0;
	//Para cada modelo, desenhar o que está no path + model
	while (counter<list_models_counter)
	{
		model_name = concatCharsUntil(model_name, i);
		std::string line;
		std::string path_model = path_3d + model_name;
		ifstream myfile(path_model);
		float x, y, z;
		if (myfile.is_open())
		{

			glBegin(GL_TRIANGLES);
			while (getline(myfile, line))
			{
				//Ignorar as caracterizações dos ficheiros 3d
				if (line[0] != '!')
				{
					//desenhar em branco
					glColor3f(1.0f, 1.0f, 1.0f);

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
			myfile.close();
		}
		model_name = "";
		counter++;
	}
}

/*
* Função que vai gerar os axis
*/
void drawAxis()
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


/*
* Algoritmo para extrair o xml para as variaveis globais do codigo
*/
void loadXML(void)
{
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
				pPosition->QueryFloatAttribute("x", &posX);
				pPosition->QueryFloatAttribute("y", &posY);
				pPosition->QueryFloatAttribute("z", &posZ);

				//Formula no pdf 3, pag3
				radius = sqrt((posX * posX) + (posY * posY) + (posZ * posZ));

				//Formulas inversas no pdf 3, pag 2 e 3
				ang_beta = asin(posY / radius);
				ang_alpha = asin(posX / (radius * cos(ang_beta)));
			}

			//Tratar do lookAt
			XMLElement* pLookAt = pCamera->FirstChildElement("lookAt");
			if (pLookAt)
			{
				pLookAt->QueryFloatAttribute("x", &lookX);
				pLookAt->QueryFloatAttribute("y", &lookY);
				pLookAt->QueryFloatAttribute("z", &lookZ);
			}

			//Tratar do pUp
			XMLElement* pUp = pCamera->FirstChildElement("up");
			if (pUp)
			{
				pUp->QueryFloatAttribute("x", &upX);
				pUp->QueryFloatAttribute("y", &upY);
				pUp->QueryFloatAttribute("z", &upZ);
			}

			//Tratar da projection
			XMLElement* pProjection = pCamera->FirstChildElement("projection");
			if(pProjection)
			{
				pProjection->QueryFloatAttribute("fov", &fov);
				pProjection->QueryFloatAttribute("near", &near);
				pProjection->QueryFloatAttribute("far", &far);
			}

		}

		//Tratar dos grupos
		XMLElement* pGroup = pWorld->FirstChildElement("group");
		if (pGroup)
		{
			//Tratar os models
			XMLElement* pListModel = pGroup->FirstChildElement("models");
			if (pListModel)
			{
				//Tratar cada modelo dentro dos modelos
				XMLElement* model = pListModel->FirstChildElement("model");
				while (model != nullptr)
				{
					list_models += model->Attribute("file");
					//para cada modelo, encontrar os primos até nao existir mais primos
					model = model->NextSiblingElement("model");
					list_models_counter++;
				}
			}

		}
	}
}

#pragma region Comandos

void keyMap(unsigned char c, int x, int y) {
	// put code to process regular keys in here
	switch (c) {
	case 'p':
		trigger_on_off = (trigger_on_off + 1) % 2;
		break;
	case 'r':
		loadXML();
		camera_mode = 0;
		trigger_on_off = 0;
		break;
	case 'm':
		if (trigger_on_off)
		{
			camera_mode = (camera_mode + 1) % 2;
			reloadAng();
		}
		break;
	case '1':
		if (trigger_on_off)
		{
			if (mode_draw == GL_LINE)
			{
				mode_draw = GL_FILL;
			}
			else if (mode_draw == GL_FILL)
			{
				mode_draw = GL_POINT;
			}
			else
			{
				mode_draw = GL_LINE;
			}
		}
		break;
	case '2':
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
				mouse_trigger_on_off = (mouse_trigger_on_off+1)%2;
			}
		}
		break;
	default:
		break;
	}
}

void mouseMotion(int x, int y)
{
	float camera_rotationX = 4.5 * PI / width;
	float camera_rotationY = 3 * PI / height;
	if (camera_mode)
	{
		camera_rotationX = 10 * PI / width;
		camera_rotationY = 10 * PI / height;
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
	//load xml
	loadXML();

	// Glut init
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(width, height);
	glutCreateWindow("CG_TP16_phase1");
	
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