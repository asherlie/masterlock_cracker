#include <iostream>
#include <string>
#include <vector>

std::vector<std::vector<int> > find_digits(int resistance, int first, int second){
	int first_digit;
	std::vector<int> third, secc;

	first_digit = resistance + 5.76; //will auto cast to int
	double first_mod_4 = first_digit % 4;

	int locks[8];
	locks[0] = first;
	locks[4] = second;

	for(int i = 0; i < 8; ++i){
		if(i%4 != 0){ //to make sure i dont overwrite [0],[4]
			locks[i] = locks[i-1]+10;
		}
	}

	std::vector<int> third_possible;
	for(int i = 0; i < 40; ++i){
		if(i%4 == first_mod_4){
			third_possible.push_back(i);
		}
	}
	
	for(int i = 0; i < third_possible.size(); ++i){
		for(int j = 0; j < 8; ++j){
			if(third_possible[i] == locks[j]){
				third.push_back(third_possible[i]);
			}
		}
	}

	for(int sec = 0; sec < 40; ++sec){
		if(((sec%4)) == third[0] % 4)secc.push_back(sec + 2);
	}

	//int ret[3][1];
	std::vector<std::vector<int> > ret;
	ret.resize(3);
	ret[0].push_back(first_digit);
	for(int i = 0; i < secc.size(); ++i){
		ret[1].push_back(secc[i]);
	}
	for(int i = 0; i < third.size(); ++i){
		ret[2].push_back(third[i]);		
	}
	return ret;

}

std::vector<std::vector<int> > clarify(std::vector<std::vector<int> > &res, int final_third){
	int num_to_remove = 0;
	std::vector<int> second_clarified;
	std::vector<int> third_final;
	third_final.push_back(final_third);
	for(int i = 0; i < res[1].size(); ++i){
		if(!(res[1][i] == final_third + 2 || res[1][i] == final_third - 2)){
			second_clarified.push_back(res[1][i]);
		}
	}
	res[1] = second_clarified;
	res[2] = third_final;
	return res;
}

std::string vec_to_str(std::vector<int> inp){
	std::cout << "sze" << inp.size() << std::endl;
	std::string str = "";
	for(int i = 0; i < inp.size(); ++i){
		str += std::to_string(inp[i]);
		if(i != inp.size()-1)str += ", ";
	}
	return str;
}
int main(int argc, char* argv[]){
	if(argc < 4)return -1;
	if(argc >= 4){
		std::vector<std::vector<int> > res = find_digits(std::stoi(argv[1]), std::stoi(argv[2]), std::stoi(argv[3]));
		if(argc > 4){
			clarify(res, std::stoi(argv[4]));
		}
		std::cout << "first: " << vec_to_str(res[0]) << "\nsecond: " << vec_to_str(res[1]) << "\nthird: " << vec_to_str(res[2]) << std::endl;
	}
}
