int data_items[]={3,67,222,45,75,54,34,44,33,22,11,66,0 };

int main(void)
{
    int max;
    int* pointer;
    pointer=&data_items[0];
    max=data_items[0];
    while (*pointer!=0)
    {
        if(*pointer>max)
        {
            max=*pointer;
        }
        pointer++;
    }
    return max;
}
