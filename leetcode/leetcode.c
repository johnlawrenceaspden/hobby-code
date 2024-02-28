#include<stdio.h>
#include<malloc.h>
#include<assert.h>
#include<stdlib.h>

/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
int* twoSum(int* nums, int numsSize, int target, int* returnSize);




void aiprint(int *array, int arraysize)
{
  printf("{");
  for(int i=0; i<arraysize-1; i++){
    printf("%d,", array[i]);
  }
  printf("%d}", array[arraysize-1]);
}



int comparator(const void *p, const void *q) 
{ 
  return (*(int*)p-*(int*)q);
} 






int* twoSum_sort_lookup(int* nums, int numsSize, int target, int* returnSize) {
  //printf("---\n");

  int *newnums=malloc(sizeof(int)*numsSize);
  for(int i=0; i<numsSize; i++) newnums[i]=nums[i];
  qsort((void*)newnums, numsSize, sizeof(int), comparator);

  //aiprint(nums, numsSize); printf("\n");
  //aiprint(newnums, numsSize); printf("\n");
  
  int* i = newnums ; 
  int* j = newnums + numsSize-1; 
  //printf("%d + %d = %d\n", *i, *j, *i + *j);
  while (*i+*j != target && j>=i) {
    if (*i + *j > target){ j--;}
    else {i++;}
    //printf("%d + %d = %d\n", *i, *j, *i + *j);
  }
  int a=*i;
  int b=*j;
  free(newnums);

  int* p1=nums;
  int* p2=nums+numsSize-1;
  for( ;*p1!=a; p1++); //{printf("%d->",*p1);}
  //printf("%d\n",*p1);
  for( ;*p2!=b; p2--); //{printf("%d->",*p2);}
  //printf("%d\n",*p2);
    
  int* retval=(int*) malloc(2*sizeof(int));
  *returnSize=2;
  retval[0]=p1-nums;
  retval[1]=p2-nums;

  //printf("---\n");
  return retval;
}

typedef struct arrval_s{
  int val;
  int pos;
} arrval_t;

void arrvalprint(arrval_t *array, int arraysize)
{
  printf("{");
  int i;
  for(i=0; i<arraysize-1; i++){
    printf("(%d,%d),", array[i].val, array[i].pos);
  }
  printf("(%d,%d)}", array[i].val, array[i].pos);
}


int arrval_comparator(const void *p, const void *q) 
{ 
  return (((arrval_t*)p)->val - ((arrval_t*)q)->val);
} 


/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
int* twoSum(int* nums, int numsSize, int target, int* returnSize) {
  printf("---\n");
  
  int* retval=(int*) malloc(2*sizeof(int));
  *returnSize=2;
  retval[0]=-1;
  retval[1]=-1;


  arrval_t *arr = malloc(sizeof(arrval_t)*numsSize);
  for (int i=0; i<numsSize; i++){
    arr[i].val=nums[i]; arr[i].pos=i;
  }

  //arrvalprint(arr,numsSize); printf("\n");
  qsort(arr, numsSize, sizeof(arrval_t), arrval_comparator);
  //arrvalprint(arr,numsSize); printf("\n");

  
  arrval_t* i = arr ; 
  arrval_t* j = arr + numsSize-1; 
  //printf("%d + %d = %d\n", *i, *j, *i + *j);
  for(;;){
    int sum = i->val+j->val;
    if (sum==target) break;
    else if (sum>target) j--;
    else i++;
  }
  retval[0]=i->pos;
  retval[1]=j->pos;
  
  free(arr);

  printf("---\n");
  return retval;
}


void twoSumTest(int*nums, int numsize, int target)
{
  int* retval;
  int returnSize;
  printf("---\n");
  aiprint(nums, numsize);
  printf(" => %d\n", target);
  retval = twoSum(nums, numsize, target, &returnSize);
  aiprint(retval, returnSize); printf("\n");

  int a=*(nums+retval[0]);
  int b=*(nums+retval[1]);
  int c = a+b;
  printf("%d + %d = %d\n",a,b,c);
  free(retval);
  if(target==a+b) printf("yay\n"); else printf("BOO!\n");
}



int main(void)
{
  printf("yo\n");

  {
    int nums[]={2,7,11,15};
    int target=9;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }

  {
    int nums[]={3,2,4};
    int target=6;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }

  {
    int nums[]={3,3};
    int target=6;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }


  
  {
    int nums[]={2,7,11,15};
    int target=14;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }


  {
    int nums[]={2,7,11,15};
    int target=4;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }


  {
    int nums[]={3,3};
    int target=6;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }

  {
    int nums[]={-1,-2,-3,-4,-5};
    int target=-8;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }



  
  return 0;
}
