class Sun {
  private float posX = 640;
  private float posY= 360;
  private float radius=150;
  private int r=255; 
  private int g=174; 
  private int b=66;

  void render() {
    pushMatrix();
    translate(posX, posY);
    stroke(0);
    strokeWeight(2);
    fill(this.r, this.g, this.b); 
    ellipse(0, 0, radius*2, radius*2);
    popMatrix();
  }
}
