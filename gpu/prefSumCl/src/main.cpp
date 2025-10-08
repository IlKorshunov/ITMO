#include "sum.h"

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

int main(int argc, char** argv) {
    try {
        CommandLineArgs args = parse_args(argc, argv);
        std::vector<uint32_t> input;
        //read_text_input(args.input_file, input);
        read_input(args.input_file, input);
        OptimalConstants constants = calculateOptimalConstants(input.size(), args);
        std::string build_opts = generateBuildOpts(constants);
        DeviceInfo dev = selectDevice(args.device_type, args.device_index);
        Ocl ocl = Ocl::build(dev, "prefix_sum.cl", build_opts.c_str());
        std::vector<uint32_t> output;
        Timings t = run_prefix_sum(ocl, input, output, constants);
        print_timings(t, constants, ocl.device_name, ocl.platform_name);
        write_output(args.output_file, output);
        //write_text_output(args.output_file, output);
    } catch (const std::exception& e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
    return 0;
}
