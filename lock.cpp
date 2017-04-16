#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

std::vector<std::vector<int> > find_digits(int resistance, int first_lock, int second_lock){
	std::vector<std::vector<int> > ret; ret.resize(3);
	//i resize here to avoid a seg fault 
	int first_digit;
	std::vector<int> third, second;

	first_digit = resistance + 5.76; //will auto cast to int
	double first_mod_4 = first_digit % 4;

	int locks[8];
	locks[0] = first_lock;
	locks[4] = second_lock;

	for(int i = 0; i < 8; ++i){
		if(i%4 != 0){ // make sure [0],[4] aren't overwritten
			locks[i] = locks[i-1]+10;
		}
	}
	//numbers from 0 - 39 that are the same  %4 as the first digit%4
	std::vector<int> third_possible;
	for(int i = 0; i < 40; ++i){
		if(i%4 == first_mod_4){
			third_possible.push_back(i);
		}
	}
	//cross reference nums that are the same %4 with locks[]
	for(int i = 0; i < third_possible.size(); ++i){
		for(int j = 0; j < 8; ++j){
			if(third_possible[i] == locks[j]){
				third.push_back(third_possible[i]);
			}
		}
	}
	//if no viable third element has been found the user has made a mistake
	if(third.empty()){
		return ret;
	}

	for(int sec = 0; sec < 40; ++sec){
		if((sec%4) == (third[0] % 4))second.push_back(sec + 2);
	}

	ret[0].push_back(first_digit);
	for(int i = 0; i < second.size(); ++i){
		ret[1].push_back(second[i]);
	}
	for(int i = 0; i < third.size(); ++i){
		ret[2].push_back(third[i]);		
	}
	// replace nums above 40 and sort vectors
	for(int i = 0; i < 3; ++i){
		for(int j = 0; j < ret[i].size(); ++j){
			if(ret[i][j] >= 40){
				ret[i][j] = ret[i][j] - 40;
			}
		}
		std::sort(ret[i].begin(), ret[i].end());
	}
	return ret;

}

void clarify(std::vector<std::vector<int> > &res, int final_third){
	int num_to_remove = 0;
	std::vector<int> second_final;
	std::vector<int> third_final;
	third_final.push_back(final_third);
	for(int i = 0; i < res[1].size(); ++i){
		if(!(res[1][i] == final_third + 2 || res[1][i] == final_third - 2)){
			second_final.push_back(res[1][i]);
		}
	}
	res[1] = second_final;
	res[2] = third_final;
}

std::string vec_to_str(std::vector<int> inp){
	std::string str = "";
	for(int i = 0; i < inp.size(); ++i){
		str += std::to_string(inp[i]);
		if(i != inp.size()-1)str += ", ";
	}
	return str;
}

bool is_viable_dig(std::vector<std::vector<int> > vec){
	return !(vec[0].empty() || vec[1].empty() || vec[2].empty());
}
int main(int argc, char* argv[]){
	if(argc < 4)return -1;
	if(argc >= 4){
		std::vector<std::vector<int> > res = find_digits(std::stoi(argv[1]), std::stoi(argv[2]), std::stoi(argv[3]));
		if(!is_viable_dig(res))std::cout << "Something went wrong. Double check your measurements" << std::endl;
		else{
			if(argc > 4){
				clarify(res, std::stoi(argv[4]));
			}
			std::cout << "First: " << vec_to_str(res[0]) << "\nSecond: " << vec_to_str(res[1]) << "\nThird: " << vec_to_str(res[2]) << std::endl;
			if(argc == 4)std::cout << "To clarify results, re-run this program with the correct third digit as the fourth input" << std::endl;
		}
	}
}
