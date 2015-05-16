#include <stdio.h>
#include <stdlib.h>


typedef struct roadStruct {	
	int number;		//number of pieces of the road with the same speed limit
	int limit;		//speed limit
} roadType;

typedef struct tripStruct {	//stack for the moves that are made 
	int speed;		
	int change;		
	roadType *pstate;	//pointer to the pieces of the road where the speed changed
	struct tripStruct *prev;
} tripType;

unsigned int pieces = 0;

roadType *readFile (char *input)
{	
	FILE *fpin;
	unsigned int x = 0;
	int y = 0;
	roadType *array = NULL,*ptr = NULL;	

	fpin = fopen(input,"r");			//open file
	while (fscanf(fpin,"%d %d", &x,&y) != EOF){	//find the length of the road
		if ((x!=0) || (y!=0)){
			pieces += x;		
		}
	}
	fclose(fpin);					// close file
	array = (roadType *)malloc((pieces+2)*sizeof(roadType)); //memory allocation for the road dynamic array
	ptr=array;
	ptr->number=0;
	ptr->limit=0;
	ptr++;
	fpin = fopen(input,"r");			//open file
	while (fscanf(fpin,"%d %d", &x,&y) != EOF){	//read the file and create the dynamic array for the road
                if ((x!=0) || (y!=0)){
		   while (x>0) {
			ptr->number = x;
			ptr->limit = y;
			ptr++;
			x--;
		   }		
		}
	}
	ptr->number = 100000;			//struct for the step after the last piece of the road
        ptr->limit = 10000000;	
	fclose(fpin);
	return array;
}


int length (tripType *p){			//finds the length of the tripType list
	tripType *ptr = p;
	int i = 0;
	
	while(ptr!=NULL){
		i++;
		ptr = ptr->prev;
	}
	return i;
}

int best_moves(int a,int s, roadType *p){ 				//a--> maximum accelaration, s-->minimum slowdown
	int current_speed = 0,test = 0,exit1 = 0,exit2=0,moves = 0;
	roadType * ptr = NULL, *temp=NULL;
	tripType *head = NULL,*l = NULL, *garbage = NULL;
	

	ptr = temp = p;
	test = a;				//testing speed equals initially the maximum accelaration
	while(exit2==0) {
		exit1 = 0;
		moves = test/10; 		//moves the speed allows
		while ((exit1 == 0) && (test >= (current_speed+s))){ 	//a move has not been yet made and slowdown is greater than s
			if ((test <= ((ptr+1)->limit))){		//speed limit allows the testing speed
				if(((ptr+moves)>p+pieces+1)){           //not enough road picies for this speed
					test -= 10;
					moves = test/10;
				}
				else {
				if (moves<=(ptr+1)->number){		//the move is allowed
					exit1=1;
					l = (tripType *)malloc(sizeof(tripType)); //create new tripType struct
					l->speed = current_speed;
                                        l->change = test - current_speed;
                                        l->pstate = temp;
               			        if (head==NULL)		
                        			head = l;
                			else {
                    				l->prev = head;
                    				head = l;
                			}
					temp = ptr + moves;
					ptr += moves;
					current_speed = test;
					test = current_speed + a;	
				}
				else {					//more steps must be made with tha same speed
					moves = moves - (ptr+1)->number;
					ptr += (ptr+1)->number;
				}
				}
			}
			else{						//testing speed exceeds the limit
				test -= 10;				//reduce the testing speed
				ptr = temp;
				moves = test/10;
			} 
		}
		if (test<current_speed+s){				//backtracking
			garbage = head;					//use the tripType list to find the previous move
			ptr = head->pstate;
			head->change -= 10;
			test = head->speed + head->change;
			current_speed = head->speed;
			temp = ptr;
			head = head->prev;
			free(garbage);
		}
		if (ptr > p + pieces) 
			exit2 = 1;		
	}
	return length(head);	//moves that are finally made
	
}

int main(int argc, char* argv[])
{
   int acc,slowdown;
   roadType *head=NULL;


   if (argc!= 4) {
	printf("Usage: drive acc slowdown input.txt");
	return 1;
   }
   
   acc = atoi(argv[1]);
   slowdown = -atoi(argv[2]);
   head = readFile(argv[3]);
   printf("%d\n",best_moves(acc,slowdown,head));

   return 0;
}
