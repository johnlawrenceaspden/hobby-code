import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;

public class Send {
    public static void main(String[] argv)
        throws java.io.IOException {
        Connection conn = null;
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        conn = factory.newConnection();
        Channel chan = conn.createChannel();
        chan.queueDeclare("hello", false, false, false, null);
        chan.basicPublish("", "hello", null, "Hello, World!".getBytes());
        System.out.println(" [x] Sent 'Hello World!'");
        chan.close();
        conn.close();
    }
}

// <dependency>
//   <groupId>com.rabbitmq</groupId>
//   <artifactId>amqp-client</artifactId>
//   <version>2.2.0</version>
// </dependency>

/* Compile with:
$ javac -cp /home/john/.m2/repository/com/rabbitmq/amqp-client/2.2.0/amqp-client-2.2.0.jar Recv.java

Run with:
$ java -cp '.:/home/john/.m2/repository/com/rabbitmq/amqp-client/2.2.0/amqp-client-2.2.0.jar:/home/john/.m2/repository/commons-io/commons-io/1.4/commons-io-1.4.jar' Recv
*/