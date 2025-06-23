import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

class ClientConnection {
    private String hostName = "localhost"; 
    private int portNumber = 12345; 
    private Socket socket;
    public PrintWriter out;
    public BufferedReader in; 

    public ClientConnection(String host, int port)
    {
        this.hostName = host;
        this.portNumber = port;
    }

    public int connect() {
        try {
            //Conectar ao server
            socket = new Socket(this.hostName, this.portNumber);
            out = new PrintWriter(socket.getOutputStream());
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            System.out.println("Connected to: " +hostName +":"+portNumber);
            return 1;
        } catch (Exception e) {
            System.err.println("Erro: " + e.getMessage());
        }
        return 0;
    }
    
    public Boolean sendString(String str) {
      this.out.println(str);
      this.out.flush();
      return true;
  }
}
