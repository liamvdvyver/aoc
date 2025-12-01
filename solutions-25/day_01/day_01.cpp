#include <string>
#include <iostream>

using namespace std;

int main() {

    int ret_01 = 0;
    int ret_02 = 0;

    int mod = 100;
    int cur = 50;

    for (std::string ln; cin >> ln; ) {

        int sign = ln[0] == 'R' ? +1 : -1;
        int n = stoi(ln.substr(1));
        int prev = cur;

        cur += (n * sign);

        ret_02 += cur > 0 ? cur / mod : ((prev != 0) + (abs(cur) / mod));

        cur = ((cur % mod) + mod) % mod;

        if (!cur) ret_01++;
    }

    cout << ret_01 << '\n' << ret_02 << '\n';
}
