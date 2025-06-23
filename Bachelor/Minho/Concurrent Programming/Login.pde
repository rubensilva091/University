class Login {
  private boolean loggedIn = false;
  public char c = ' ';
  public String inputTextUsername ="";
  public String inputTextPassword ="";
  
  private char usedChar = ' ';
  public void input(char c1)
  {
    if((Character.isLetterOrDigit(c1) || c1 == BACKSPACE || c1 == DELETE)  && c1 != ' ' && c1 != usedChar)
    {
      this.c=c1;
      this.usedChar = c1;
    }
  }
  
  public int  display() {
    PImage img1 = loadImage("\\images\\backgroundLogin.png");
    image(img1, 0,0); 
    
    if (!loggedIn) {
      return displayLogin();
    } else {
      return displayLoggedIn();
    }
  }

  public void top10(List<String> leaderboard)
  {
    
float boxX = 50;
float boxY = 50;
float boxWidth =450;
float boxHeight = 400;

fill(150, 150); 
rect(boxX, boxY, boxWidth, boxHeight);

fill(255); 
textAlign(LEFT, TOP); 
textSize(30);
text("Top 10 Players",40,0);

// Add title here
fill(255); // bold in Processing/JavaFX is done by setting RGB values to 255,0,0
textAlign(LEFT, TOP); 
textSize(18); // reset text size to 14

float textX = 10;
float textY = 40;  // start text from y = 30 to leave space for the title
for (String entry : leaderboard) {
  text(entry, textX, textY);
  textY += 20; 
  }
  }

  public void usernameField()
  {
  int posX=width/2;
  int posY=(height/16)*3;
  
  
  //Retangulo do Field
  fill(200);
  rectMode(CENTER);
  float borderRadius = 20; 
  rect(posX, posY, 600, 50, borderRadius); 
  
  //Texto
  fill(255); 
  textAlign(CENTER, BOTTOM); 
  textSize(25);
  text("Username", posX-250, posY-35); 
  
  //input
  if (mouseX > posX - 300 && mouseX < posX + 300 && mouseY > posY - 25 && mouseY < posY + 25) {
    if (inputTextUsername.length()<40 && this.c != ' ')
    {
      if ((this.c == BACKSPACE || this.c == DELETE) && inputTextUsername.length()>0)
      {
         this.inputTextUsername = inputTextUsername.substring(0, inputTextUsername.length() - 1);
      }
      else
      {
      this.inputTextUsername +=this.c;
      }
    }
    this.c=' ';
  }
  fill(0);
  textSize(20);
  textAlign(LEFT, BOTTOM);
  text(inputTextUsername, posX-280, posY + 10);
  }
  
  public void passwordField()
  {
  int posX=width/2;
  int posY=((height/16)*6)-20;
  //Retangulo do Field
  fill(200);
  rectMode(CENTER);
  float borderRadius = 20; 
  rect(posX, posY, 600, 50, borderRadius); 
  
  //Texto
  fill(255); 
  textAlign(CENTER, BOTTOM); 
  textSize(25);
  text("Password", posX-250, posY-35); 
  
  //input
  if (mouseX > posX - 300 && mouseX < posX + 300 && mouseY > posY - 25 && mouseY < posY + 25) {
    if (inputTextPassword.length()<40 && this.c != ' ' )
    {
      if ((this.c == BACKSPACE || this.c == DELETE) && inputTextPassword.length()>0)
      {
         this.inputTextPassword = inputTextPassword.substring(0, inputTextPassword.length() - 1);
      }
      else
      {
      this.inputTextPassword +=this.c;
      }
    }
    this.c=' ';
  }
  fill(0);
  textSize(20);
  textAlign(LEFT, BOTTOM);
  text(inputTextPassword, posX-280, posY + 10);
  }


  public int loginButton()
  {
  int bool=2;
  //Posições genericas
  int posX=width/2;
  int posY=(height/4)*3;
  
  //Butão
  fill(0, 0, 255);
  if (mousePressed && mouseX > posX - 200 && mouseX < posX + 200 && mouseY > posY - 60 && mouseY < posY + 60)
  {
    fill(0, 0, 180);
    bool = 2;
  }
  else{
    bool = 0;}
  rectMode(CENTER);
  float borderRadius = 20;
  rect(posX, posY, 400, 120, borderRadius);
  
  //Texto dentro do Butão
  fill(255); 
  textAlign(CENTER, CENTER);
  textSize(60);
  text("Log-in", posX, posY);
  
  return bool;
  }
  
  public int CreateButton()
  {
  int bool=1;
  //Posições genericas
  int posX=(width/8)+50;
  int posY=(height/4)*3;
  
  //Butão
  fill(0, 255, 0);
  if (mousePressed && mouseX > posX - 200 && mouseX < posX + 200 && mouseY > posY - 60 && mouseY < posY + 60)
  {
    fill(0, 180, 0);
    bool = 1;
  }
  else{
    bool = 0;}
  rectMode(CENTER);
  float borderRadius = 20;
  rect(posX, posY, 400, 120, borderRadius);
  
  //Texto dentro do Butão
  fill(255); 
  textAlign(CENTER, CENTER);
  textSize(60);
  text("Create Acc", posX, posY);
  
  return bool;
  }
  
  public int DeleteButton()
  {
  int bool=3;
  //Posições genericas
  int posX=(width/8)*7-50;
  int posY=(height/4)*3;
  
  //Butão
  fill(255, 0, 0);
  if (mousePressed && mouseX > posX - 200 && mouseX < posX + 200 && mouseY > posY - 60 && mouseY < posY + 60)
  {
    fill(180, 0, 0);
    bool = 3;
  }
  else{
    bool = 0;}
  rectMode(CENTER);
  float borderRadius = 20;
  rect(posX, posY, 400, 120, borderRadius);
  
  //Texto dentro do Butão
  fill(255); 
  textAlign(CENTER, CENTER);
  textSize(60);
  text("Delete Acc", posX, posY);
  
  return bool;
  }
  
  public int buttonClickFinder()
  {
    int flag = 0;
    if(loginButton()!=0)
    {
      flag = loginButton();
    }
    if(DeleteButton() != 0)
    {
      flag = DeleteButton();
    }
    if(CreateButton()!=0)
    {
      flag = CreateButton();
    }
    return flag;
  }
  
  public void fieldClear()
  {
    this.inputTextUsername = "";
    this.inputTextPassword = "";
  }
  
  public int displayLogin() {
  passwordField();
  usernameField();
  return  buttonClickFinder();
  }
  
  public int displayLoggedIn()
  {
    return 0;
  }
}
