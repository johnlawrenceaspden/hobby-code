#include <iostream>
#include <opencv2/opencv.hpp>
#include <opencv2/highgui.hpp>

using namespace std;

int main()
{
    cv::Mat image;
    image = cv::imread("../pytorch/1nealclose.png", 1);
 
    cv::namedWindow("Display Image", cv::WINDOW_AUTOSIZE);
    cv::imshow("Display Image", image);

    cv::Mat resized;
    int new_width = 300;
    int new_height = 200;
    cv::resize(image, resized, cv::Size(new_width, new_height));
    cv::imshow("Resized Image", resized);

    int c;
    while(c = cv::waitKey(0), c!=27){
      cout << "yo" << c <<  "\n";
    }
    cv::destroyAllWindows(); 
    return 0;
}
