#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
#include <vector>
#include "sum.h"

std::vector<cl_platform_id> getPlatforms() {
    cl_uint count_plats = 0;
    check_error(clGetPlatformIDs(0, nullptr, &count_plats), "failed count platforms");
    if (!count_plats) throw std::runtime_error("No OpenCL platforms");
    std::vector<cl_platform_id> plats(count_plats);
    check_error(clGetPlatformIDs(count_plats, plats.data(), nullptr), "failed get list platforms");
    return plats;
}

std::vector<cl_device_id> getDevices(cl_platform_id platform) {
    cl_uint count_devs = 0;
    check_error(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 0, nullptr, &count_devs), "failed count devices");
    if (!count_devs) throw std::runtime_error("No OpenCL devices on selected platform");
    std::vector<cl_device_id> devices(count_devs);
    check_error(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, count_devs, devices.data(), nullptr), "failed get list devices");
    return devices;
}

static int isIGPU(cl_device_id device) {
    cl_bool res;
    clGetDeviceInfo(device, CL_DEVICE_HOST_UNIFIED_MEMORY, sizeof(cl_bool), &res, NULL);
    return (res == CL_TRUE) ? 1 : 0;
}

int prio(DeviceType t) {
    switch (t) {
    case DeviceType::DGPU: return 0;
    case DeviceType::IGPU: return 1;
    case DeviceType::CPU: return 2;
    default: return 3;
    }
}

std::vector<DeviceInfo> getAllDevicesSorted() {
    std::vector<cl_platform_id> platforms = getPlatforms();
    std::vector<DeviceInfo> all_devices;

    for (auto platform : platforms) {
        auto devices = getDevices(platform);

        std::string platform_name_str = getInfoString(platform, clGetPlatformInfo, CL_PLATFORM_NAME);

        for (auto device : devices) {
            cl_device_type dtype;
            clGetDeviceInfo(device, CL_DEVICE_TYPE, sizeof(dtype), &dtype, nullptr);
            std::string device_name_str = getInfoString(device, clGetDeviceInfo, CL_DEVICE_NAME);
            DeviceType type;
            if (dtype & CL_DEVICE_TYPE_CPU) {
                type = DeviceType::CPU;
            } else if (dtype & CL_DEVICE_TYPE_GPU) {
                if (isIGPU(device)) {
                    type = DeviceType::IGPU;
                } else {
                    type = DeviceType::DGPU;
                }
            } else {
                type = DeviceType::ANY;
            }

            all_devices.emplace_back(platform, device, device_name_str, platform_name_str, type);
        }
    }

    std::sort(all_devices.begin(), all_devices.end(), [](auto& a, auto& b) { return prio(a.type) < prio(b.type); });
    return all_devices;
}

DeviceType stringToDeviceType(const std::string& type_str) {
    if (type_str == "dgpu") return DeviceType::DGPU;
    if (type_str == "igpu") return DeviceType::IGPU;
    if (type_str == "gpu") return DeviceType::GPU;
    if (type_str == "cpu") return DeviceType::CPU;
    if (type_str == "all") return DeviceType::ANY;
    throw std::runtime_error("Invalid device type: " + type_str);
}

DeviceInfo selectDevice(const std::string& device_type, int device_index) {
    auto all_devices = getAllDevicesSorted();

    if (all_devices.empty()) { throw std::runtime_error("No OpenCL devices available"); }

    std::vector<DeviceInfo> filtered_devices;
    if (device_type != "all") {
        DeviceType target_type = stringToDeviceType(device_type);
        for (const auto& device : all_devices) {
            if (device.type == target_type) {
                filtered_devices.push_back(device);
            } else if (target_type == DeviceType::GPU && (device.type == DeviceType::DGPU || device.type == DeviceType::IGPU)) {
                filtered_devices.push_back(device);
            }
        }
    } else {
        filtered_devices = all_devices;
    }

    if (filtered_devices.empty()) { throw std::runtime_error("No devices of type " + device_type + " available"); }

    int selected_index;
    if (device_index < 0 || device_index >= (int)filtered_devices.size()) {
        selected_index = 0;
    } else {
        selected_index = device_index;
    }

    return filtered_devices[selected_index];
}
