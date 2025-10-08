#include <cuda_runtime.h>
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include "mult.h"

static int isIGPU(const cudaDeviceProp& p) { return p.integrated ? 1 : 0; }

DeviceType stringToDeviceType(const std::string& type_str) {
    if (type_str == "dgpu") return DeviceType::DGPU;
    if (type_str == "igpu") return DeviceType::IGPU;
    if (type_str == "gpu") return DeviceType::GPU;
    if (type_str == "cpu") return DeviceType::CPU;
    if (type_str == "all") return DeviceType::ANY;
    throw std::runtime_error("Invalid device type: " + type_str);
}

std::vector<DeviceInfo> getAllDevicesSorted() {
    int count = 0;
    cuda_check(cudaGetDeviceCount(&count), "cudaGetDeviceCount");
    if (count <= 0) throw std::runtime_error("No CUDA devices available");

    std::vector<DeviceInfo> all_devices;
    all_devices.reserve(count);

    for (int i = 0; i < count; ++i) {
        cudaDeviceProp p{};
        cuda_check(cudaGetDeviceProperties(&p, i), "cudaGetDeviceProperties");

        int driverVersion = 0;
        cuda_check(cudaDriverGetVersion(&driverVersion), "cudaDriverGetVersion");

        DeviceInfo info;
        info.device_index = i;
        info.device_name = p.name;
        info.driverVersion = driverVersion;
        info.type = isIGPU(p) ? DeviceType::IGPU : DeviceType::DGPU;

        all_devices.emplace_back(std::move(info));
    }

    std::sort(all_devices.begin(), all_devices.end(), [](const DeviceInfo& a, const DeviceInfo& b) {
        auto prio = [](DeviceType t) {
            switch (t) {
            case DeviceType::DGPU: return 0;
            case DeviceType::IGPU: return 1;
            case DeviceType::CPU: return 2;
            default: return 3;
            }
        };
        int pa = prio(a.type);
        int pb = prio(b.type);
        if (pa != pb) return pa < pb;
        return a.device_index < b.device_index;
    });

    return all_devices;
}

DeviceInfo selectDevice(const std::string& device_type, int device_index) {
    auto all_devices = getAllDevicesSorted();
    if (all_devices.empty()) throw std::runtime_error("No CUDA devices available");

    std::vector<DeviceInfo> filtered_devices;
    if (device_type != "all") {
        DeviceType target_type = stringToDeviceType(device_type);
        for (const auto& dev : all_devices) {
            if (dev.type == target_type) {
                filtered_devices.push_back(dev);
            } else if (target_type == DeviceType::GPU && (dev.type == DeviceType::DGPU || dev.type == DeviceType::IGPU)) {
                filtered_devices.push_back(dev);
            }
        }
    } else {
        filtered_devices = all_devices;
    }

    if (filtered_devices.empty()) { throw std::runtime_error("No devices of type " + device_type + " available"); }

    int selected_index = (device_index < 0 || device_index >= (int)filtered_devices.size()) ? 0 : device_index;
    return filtered_devices[selected_index];
}