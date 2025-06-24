#include <iostream>
#include <string.h>
#include <fstream>
#include <sstream>
#include <math.h>

#define PI 2 * acos(0.0)
using namespace std;

/*
Função responsável por desenhar os vertices dos triangulos do torus
*/
std::string drawTorus(float innerRadius, float outerRadius, int sides, int rings)
{

    std::stringstream descricao;
    std::stringstream list;
    int triangulos =0;
    //Descrição do torus
    descricao << "!!GraphicalPrimitive: torus"<< '\n';
    descricao << "!!Inner Radius: " <<innerRadius <<'\n';
    descricao << "!!Outer Radius: " <<outerRadius<< '\n';
    descricao << "!!Sides: " << sides<<'\n';
    descricao << "!!Rings: " << rings<<'\n';

    //Calculo do Torus
    float ring = (2 * PI) / rings;
    float side = (2 * PI) / sides;

    float radius_torus = (outerRadius - innerRadius) / 2;
    float center_torus = innerRadius + radius_torus;

    for (int i = 0; i < rings; i++)
    {
        for (int j = 0; j < sides; j++)
        {

            float y = radius_torus * sin(i * ring);
            float next_y = radius_torus * sin((i + 1) * ring);

            float x1 = (center_torus + radius_torus * cos(i * ring)) * cos(j * side);
            float z1 = (center_torus + radius_torus * cos(i * ring)) * sin(j * side);
            float next_x1 = (center_torus + radius_torus * cos(i * ring)) * cos((j + 1) * side);
            float next_z1 = (center_torus + radius_torus * cos(i * ring)) * sin((j + 1) * side);

            float x2 = (center_torus + radius_torus * cos((i + 1) * ring)) * cos(j * side);
            float z2 = (center_torus + radius_torus * cos((i + 1) * ring)) * sin(j * side);
            float next_x2 = (center_torus + radius_torus * cos((i + 1) * ring)) * cos((j + 1) * side);
            float next_z2 = (center_torus + radius_torus * cos((i + 1) * ring)) * sin((j + 1) * side);

            // primeiro triângulo
            list << x1 << ' ' << y << ' ' << z1 << '\n';
            list << x2 << ' ' << next_y << ' ' << z2 << '\n';
            list << next_x1 << ' ' << y << ' ' << next_z1 << '\n';
            // segundo triângulo
            list << x2 << ' ' << next_y << ' ' << z2 << '\n';
            list << next_x2 << ' ' << next_y << ' ' << next_z2 << '\n';
            list << next_x1 << ' ' << y << ' ' << next_z1 << '\n';

            triangulos+=2;
        }
    }
    descricao << "!!Triangulos Desenhados: "<< triangulos<<'\n';
    descricao<<list.str();
    return descricao.str();
}

/*
Função responsável por desenhar os vertices dos triangulos da esfera
*/
std::string drawSphere(float radius, int slices, int stacks)
{

    std::stringstream descricao;
    std::stringstream list;
    int triangulos =0;
    //Descrição da Esferra
    descricao << "!!GraphicalPrimitive: sphere"<< '\n';
    descricao << "!!Radius: " << radius <<'\n';
    descricao << "!!Slices: " << slices << '\n';
    descricao << "!!Stacks: " << stacks <<'\n';

    //Calculo da Esfera
    float slice = 2 * PI / slices; // representa o alpha
    float stack = PI / stacks;     // representa o beta

    float PI_2 = PI / 2;
    for (int i = 0; i < slices; i++)
    {
        for (int j = 0; j < stacks; j++)
        {
            float x1 = radius * cos(PI_2 - stack * j) * sin(slice * i);
            float z1 = radius * cos(PI_2 - stack * j) * cos(slice * i);
            float x2 = radius * cos(PI_2 - stack * (j + 1)) * sin(slice * i);
            float z2 = radius * cos(PI_2 - stack * (j + 1)) * cos(slice * i);

            float next_x1 = radius * cos(PI_2 - stack * j) * sin(slice * (i + 1));
            float next_z1 = radius * cos(PI_2 - stack * j) * cos(slice * (i + 1));
            float next_x2 = radius * cos(PI_2 - stack * (j + 1)) * sin(slice * (i + 1));
            float next_z2 = radius * cos(PI_2 - stack * (j + 1)) * cos(slice * (i + 1));

            float y = radius * sin(PI_2 - stack * j);
            float next_y = radius * sin(PI_2 - stack * (j + 1));

            // Triangulos da esquerda (baixo pra cima)
            list << x2 << ' ' << next_y << ' ' << z2 << '\n';
            list << next_x2 << ' ' << next_y << ' ' << next_z2 << '\n';
            list << next_x1 << ' ' << y << ' ' << next_z1 << '\n';

            // Triangulos da direita (cima pra baixo)
            list << x1 << ' ' << y << ' ' << z1 << '\n';
            list << x2 << ' ' << next_y << ' ' << z2 << '\n';
            list << next_x1 << ' ' << y << ' ' << next_z1 << '\n';
            triangulos+=2;
        }
    }
    descricao << "!!Triangulos Desenhados: "<< triangulos<<'\n';
    descricao<<list.str();
    return descricao.str();
}

/*
Função responsável por desenhar os vertices dos triangulos do cubo
*/
std::string drawBox(float dimension, int divisions)
{

    std::stringstream descricao;
    std::stringstream list;
    int triangulos =0;
    //Descrição da Box
    descricao << "!!GraphicalPrimitive: box"<< '\n';
    descricao << "!!Dimension: " << dimension <<'\n';
    descricao << "!!Divisions: " << divisions << '\n';

    //Calculo da Box
    float divisions_lenght = dimension / divisions;
    float trans = dimension / 2;
    // Base cima e de baixo
    for (int i = 0; i < divisions; i++)
    {
        // para deslocarmos o plano para a origem
        float z = (divisions_lenght * i) - trans;
        float next_z = (divisions_lenght * (i + 1)) - trans;
        for (int j = 0; j < divisions; j++)
        {
            float x = (divisions_lenght * j) - trans;
            float next_x = (divisions_lenght * (j + 1)) - trans;

            // Parte de cima
            // Triangulo do lado esquerdo
            list << x << ' ' << -trans << ' ' << z << '\n';
            list << next_x << ' ' << -trans << ' ' << z << '\n';
            list << x << ' ' << -trans << ' ' << next_z << '\n';

            // Triangulo do lado esquerdo
            list << next_x << ' ' << -trans << ' ' << z << '\n';
            list << next_x << ' ' << -trans << ' ' << next_z << '\n';
            list << x << ' ' << -trans << ' ' << next_z << '\n';

            // Parte de baixo
            // Triangulo do lado esquerdo
            list << x << ' ' << dimension - trans << ' ' << z << '\n';
            list << x << ' ' << dimension - trans << ' ' << next_z << '\n';
            list << next_x << ' ' << dimension - trans << ' ' << z << '\n';

            // Triangulo do lado esquerdo
            list << next_x << ' ' << dimension - trans << ' ' << z << '\n';
            list << x << ' ' << dimension - trans << ' ' << next_z << '\n';
            list << next_x << ' ' << dimension - trans << ' ' << next_z << '\n';
            triangulos+=4;
        }
    }

    // Lateral 1 e 3
    for (int i = 0; i < divisions; i++)
    {
        // para deslocarmos o plano para a origem
        float z = (divisions_lenght * i) - trans;
        float next_z = (divisions_lenght * (i + 1)) - trans;
        for (int j = 0; j < divisions; j++)
        {
            float y = (divisions_lenght * j) - trans;
            float next_y = (divisions_lenght * (j + 1)) - trans;

            // Parte de cima
            // Triangulo do lado esquerdo
            list << -trans << ' ' << y << ' ' << z << '\n';
            list << -trans << ' ' << y << ' ' << next_z << '\n';
            list << -trans << ' ' << next_y << ' ' << z << '\n';

            // Triangulo do lado esquerdo
            list << -trans << ' ' << next_y << ' ' << z << '\n';
            list << -trans << ' ' << y << ' ' << next_z << '\n';
            list << -trans << ' ' << next_y << ' ' << next_z << '\n';

            // Parte de Baixo
            // Triangulo do lado esquerdo
            list << dimension - trans << ' ' << y << ' ' << z << '\n';
            list << dimension - trans << ' ' << next_y << ' ' << z << '\n';
            list << dimension - trans << ' ' << y << ' ' << next_z << '\n';

            // Triangulo do lado esquerdo
            list << dimension - trans << ' ' << next_y << ' ' << z << '\n';
            list << dimension - trans << ' ' << next_y << ' ' << next_z << '\n';
            list << dimension - trans << ' ' << y << ' ' << next_z << '\n';
            triangulos+=4;
        }
    }

    // Lateral 2 e 4
    for (int i = 0; i < divisions; i++)
    {
        // para deslocarmos o plano para a origem
        float x = (divisions_lenght * i) - trans;
        float next_x = (divisions_lenght * (i + 1)) - trans;
        for (int j = 0; j < divisions; j++)
        {
            float y = (divisions_lenght * j) - trans;
            float next_y = (divisions_lenght * (j + 1)) - trans;

            // Parte de cima
            // Triangulo do lado esquerdo
            list << x << ' ' << y << ' ' << -trans << '\n';
            list << x << ' ' << next_y << ' ' << -trans << '\n';
            list << next_x << ' ' << y << ' ' << -trans << '\n';

            // Triangulo do lado esquerdo
            list << x << ' ' << next_y << ' ' << -trans << '\n';
            list << next_x << ' ' << next_y << ' ' << -trans << '\n';
            list << next_x << ' ' << y << ' ' << -trans << '\n';

            // Parte de Baixo
            // Triangulo do lado esquerdo
            list << x << ' ' << y << ' ' << dimension - trans << '\n';
            list << next_x << ' ' << y << ' ' << dimension - trans << '\n';
            list << x << ' ' << next_y << ' ' << dimension - trans << '\n';

            // Triangulo do lado esquerdo
            list << x << ' ' << next_y << ' ' << dimension - trans << '\n';
            list << next_x << ' ' << y << ' ' << dimension - trans << '\n';
            list << next_x << ' ' << next_y << ' ' << dimension - trans << '\n';
            triangulos+=4;
        }
    }
    descricao << "!!Triangulos Desenhados: "<< triangulos<<'\n';
    descricao<<list.str();
    return descricao.str();
}

/*
Função responsável por desenhar os vertices dos triangulos do plano
*/
std::string drawPlane(float lenght, int divisions)
{

    std::stringstream descricao;
    std::stringstream list;
    int triangulos =0;
    //Descrição do plani
    descricao << "!!GraphicalPrimitive: plane"<< '\n';
    descricao << "!!Lenght: " << lenght <<'\n';
    descricao << "!!Divisions: " << divisions << '\n';

    //Calculo do Plano
    float divisions_lenght = lenght / divisions;
	float trans = lenght/2;
	for (int i = 0; i < divisions; i++)
	{
		//para deslocarmos o plano para a origem

		float z = (divisions_lenght * i) - trans;
		float next_z = (divisions_lenght * (i+1)) - trans;
		for (int j = 0; j < divisions; j++)
		{
			float x = (divisions_lenght * j) - trans;
			float next_x = (divisions_lenght * (j + 1)) - trans;

			//Triangulo do lado esquerdo
            list << x << ' ' << 0 << ' ' << z << '\n';
            list << x << ' ' << 0 << ' ' << next_z << '\n';
            list << next_x << ' ' << 0 << ' ' << z << '\n';

			//Triangulo do lado esquerdo
            list << next_x << ' ' << 0 << ' ' << z << '\n';
            list << x << ' ' << 0 << ' ' << next_z << '\n';
            list << next_x << ' ' << 0 << ' ' << next_z << '\n';
            triangulos+=2;
		}
	}
    descricao << "!!Triangulos Desenhados: "<< triangulos<<'\n';
    descricao<<list.str();
    return descricao.str();
}

/*
Função responsável por desenhar os vertices dos triangulos do cone
*/
std::string drawCone(float radius, float height, int slices, int stacks)
{

    std::stringstream descricao;
    std::stringstream list;
    int triangulos =0;
    //Descrição do Cone
    descricao << "!!GraphicalPrimitive: cone"<< '\n';
    descricao << "!!Radius: " << radius <<'\n';
    descricao << "!!Height: " << height << '\n';
    descricao << "!!Slices: " << slices << '\n';
    descricao << "!!Stacks: " << stacks << '\n';

    //Calculo do Cone
    float slice = (2 * PI) / slices;

    // Lateral
    float height_stack = height / stacks;
    for (int i = 0; i < stacks; i++)
    {
        for (int j = 0; j < slices; j++)
        {
            float ang1 = slice * j;
            float ang2 = ang1 + slice;

            // Alturas das stacks
            float y = i * height_stack;
            float next_y = (i + 1) * height_stack;

            // Pontos do cilindro
            float x1 = radius * sin(ang1) * (stacks - i) / stacks;
            float z1 = radius * cos(ang1) * (stacks - i) / stacks;
            float next_x1 = radius * sin(ang2) * (stacks - i) / stacks;
            float next_z1 = radius * cos(ang2) * (stacks - i) / stacks;

            // Pontos do cilindro
            float x2 = radius * sin(ang1) * (stacks - i - 1) / stacks;
            float z2 = radius * cos(ang1) * (stacks - i - 1) / stacks;
            float next_x2 = radius * sin(ang2) * (stacks - i - 1) / stacks;
            float next_z2 = radius * cos(ang2) * (stacks - i - 1) / stacks;

            // Triangulos da esquerda
            list << x2 << ' ' << next_y << ' ' << z2 << '\n';
            list << next_x1 << ' ' << y << ' ' << next_z1 << '\n';
            list << next_x2 << ' ' << next_y << ' ' << next_z2 << '\n';

            // Triangulos da direita
            list << x1 << ' ' << y << ' ' << z1 << '\n';
            list << next_x1 << ' ' << y << ' ' << next_z1 << '\n';
            list << x2 << ' ' << next_y << ' ' << z2 << '\n';
            triangulos+=2;
        }
    }

    // Base
    for (int i = 0; i < slices; i++)
    {
        // Radianos da base do cilindro
        float ang1 = slice * i;
        float ang2 = ang1 + slice;

        // Pontos do cilindro
        float x = radius * sin(ang1);
        float z = radius * cos(ang1);
        float next_x = radius * sin(ang2);
        float next_z = radius * cos(ang2);

        // Cada triangulo
        list << 0 << ' ' << 0 << ' ' << 0 << '\n';
        list << next_x << ' ' << 0 << ' ' << next_z << '\n';
        list << x << ' ' << 0 << ' ' << z << '\n';
        triangulos+=1;
    }
    descricao << "!!Triangulos Desenhados: "<< triangulos<<'\n';
    descricao<<list.str();
    return descricao.str();
}

/*
Função responsável por desenhar os vertices dos triangulos do cilindro
*/
std::string drawCylinder(float radius, float height, int slices, int stacks)
{

    std::stringstream descricao;
    std::stringstream list;
    int triangulos =0;
    //Descrição do Cilindro
    descricao << "!!GraphicalPrimitive: cylinder"<< '\n';
    descricao << "!!Radius: " << radius <<'\n';
    descricao << "!!Height: " << height << '\n';
    descricao << "!!Slices: " << slices << '\n';
    descricao << "!!Stacks: " << stacks << '\n';

    //Calculo do cilindro
    float slice = (2 * PI) / slices;

    // Bases
    for (int i = 0; i < slices; i++)
    {
        // Radianos da base do cilindro
        float ang1 = slice * i;
        float ang2 = ang1 + slice;

        // Pontos do cilindro
        float x = radius * sin(ang1);
        float z = radius * cos(ang1);
        float next_x = radius * sin(ang2);
        float next_z = radius * cos(ang2);

        // Base de baixo
        list << 0 << ' ' << 0 << ' ' << 0 << '\n';
        list << next_x << ' ' << 0 << ' ' << next_z << '\n';
        list << x << ' ' << 0 << ' ' << z << '\n';

        // Base de cima
        list << 0 << ' ' << height << ' ' << 0 << '\n';
        list << x << ' ' << height << ' ' << z << '\n';
        list << next_x << ' ' << height << ' ' << next_z << '\n';
        triangulos+=2;
    }
    // Lateral
    float height_stack = height / stacks;
    for (int i = 0; i < stacks; i++)
    {
        for (int j = 0; j < slices; j++)
        {
            float ang1 = slice * j;
            float ang2 = ang1 + slice;

            // Alturas das stacks
            float y = i * height_stack;
            float next_y = (i + 1) * height_stack;

            // Pontos do cilindro
            float x = radius * sin(ang1);
            float z = radius * cos(ang1);
            float next_x = radius * sin(ang2);
            float next_z = radius * cos(ang2);

            // Triangulos da esquerda
            list << x << ' ' << next_y << ' ' << z << '\n';
            list << x << ' ' << y << ' ' << z << '\n';
            list << next_x << ' ' << next_y << ' ' << next_z << '\n';

            // Triangulos da direita
            list << x << ' ' << y << ' ' << z << '\n';
            list << next_x << ' ' << y << ' ' << next_z << '\n';
            list << next_x << ' ' << next_y << ' ' << next_z << '\n';
            triangulos+=2;
        }
    }
    descricao << "!!Triangulos Desenhados: "<< triangulos<<'\n';
    descricao<<list.str();
    return descricao.str();
}

/*
Função responsável por executar a função correta do generator
*/
std::string generator(int argc, char const *argv[])
{
    std::string draw;
    if (!strcmp(argv[1], "cylinder"))
    {
        draw = drawCylinder(std::stof(argv[2]), std::stof(argv[3]), std::stoi(argv[4]), std::atoi(argv[5]));
    }
    else if (!strcmp(argv[1], "cone"))
    {
        draw = drawCone(std::stof(argv[2]), std::stof(argv[3]), std::stoi(argv[4]), std::atoi(argv[5]));
    }
    else if (!strcmp(argv[1], "sphere"))
    {
        draw = drawSphere(std::stof(argv[2]), std::stoi(argv[3]), std::stoi(argv[4]));
    }
    else if (!strcmp(argv[1], "box"))
    {
        draw = drawBox(std::stof(argv[2]), std::stoi(argv[3]));
    }
    else if (!strcmp(argv[1], "plane"))
    {
        draw = drawPlane(std::stof(argv[2]), std::stoi(argv[3]));
    }
    else if (!strcmp(argv[1], "torus"))
    {
        draw = drawTorus(std::stof(argv[2]), std::stof(argv[3]), std::stoi(argv[4]), std::atoi(argv[5]));
    }
    return draw;
}

/*
Função responsável por verificar se o input é Float.
É obrigatório encontrar exatamente um ponto e este não pode estar no inicio nem no fim
*/
int isFloat(char const *s)
{
    int ponto_decimal = 0;
    int i;
    for (i = 0; s[i]; i++)
    {

        /*Verificar se é digito, e se não for, verificar se é ponto*/
        if (!isdigit(s[i]))
        {
            if (i != 0 && ponto_decimal == 0)
            {
                if (s[i] == '.')
                {
                    ponto_decimal++;
                }
            }
            else
            {
                return 0;
            }
        }
    }

    /*Caso exista mais que um ponto ou que não tenhamos dito a parte decimal do numero*/
    if (ponto_decimal != 1 || s[i - 1] == '.')
    {
        return 0;
    }
    return 1;
}

/*
Função responsável por verificar se o input é inteiro.
*/
int isInteger(char const *s)
{
    for (int i = 0; s[i]; i++)
    {
        if (!isdigit(s[i]))
        {
            return 0;
        }
    }
    return 1;
}

/*
Função responsável por verificar se o input é válido (Nome da primitiva grafica + a quantidade certa de inputs).
As primitivas gráficas atuais são:
cylinder, cone, sphere, cube, plane
o argc é a quantidade de paramatros do argv + 1
*/
int input_validator(int argc, char const *argv[])
{
    int flag = -1;
    if (!strcmp(argv[1], "cylinder") && argc == 7 ||
        !strcmp(argv[1], "cone") && argc == 7 ||
        !strcmp(argv[1], "torus") && argc == 7 ||
        !strcmp(argv[1], "sphere") && argc == 6 ||
        !strcmp(argv[1], "box") && argc == 5 ||
        !strcmp(argv[1], "plane") && argc == 5)
    {
        flag++;
        /*Ver se os parametros são inteiros ou floats*/
        for (int i = 2; argv[i] && i < argc - 1; i++)
        {
            if (!isInteger(argv[i]) && !isFloat(argv[i]))
            {
                std::cout << "A dimensao " << argv[i] << " nao e valida" << std::endl;
                flag++;
            }
        }
    }

    if (!flag)
    {
        std::cout << "Input Valido!" << std::endl;
        return 1;
    }
    std::cout << "Input Invalido" << std::endl;
    return 0;
}

int main(int argc, char const *argv[])
{
    int input_valid = input_validator(argc, argv);
    if (input_valid)
    {
        /*Cria o ficheiro*/
        ofstream MyFile(argv[argc - 1]);

        /*Escreve no ficheiro*/
        MyFile << generator(argc, argv);

        /*Close the file*/
        MyFile.close();
    }
}