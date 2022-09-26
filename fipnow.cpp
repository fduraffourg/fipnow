#include <curl/curl.h>
#include <iostream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

#define COLOR_BOLD "\e[1m"
#define COLOR_OFF "\e[m"

const char *URL =
    "https://www.fip.fr/latest/api/"
    "graphql?operationName=Now&variables=%7B%22bannerPreset%22%3A%22600x600-"
    "noTransform%22%2C%22stationId%22%3A7%2C%22previousTrackLimit%22%3A3%7D&"
    "extensions=%7B%22persistedQuery%22%3A%7B%22version%22%3A1%2C%22sha256Hash%"
    "22%3A%228a931c7d177ff69709a79f4c213bd2403f0c11836c560bc22da55628d8100df8%"
    "22%7D%7D";

static size_t WriteCallback(void *contents, size_t size, size_t nmemb,
                            void *userp) {
  ((std::string *)userp)->append((char *)contents, size * nmemb);
  return size * nmemb;
}

int main() {
  CURL *curl;
  CURLcode res;
  std::string readBuffer;

  curl = curl_easy_init();
  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, URL);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
  }

  json j;
  try {
    j = json::parse(readBuffer);
  } catch (json::parse_error &e) {
    std::cerr << "json parse error: " << e.what() << std::endl;
  }

  auto data = j["data"];

  for (auto &el : data["previousTracks"]["edges"].items()) {
    auto node = el.value()["node"];
    std::cout << "   - " << node["title"].get<std::string>() << " | "
              << node["subtitle"].get<std::string>() << std::endl;
  }

  auto now = data["now"]["playing_item"];
  std::cout << COLOR_BOLD << " --> " << now["title"].get<std::string>() << " | "
            << now["subtitle"].get<std::string>() << COLOR_OFF << std::endl;

  for (auto &el : data["nextTracks"].items()) {
    auto node = el.value();
    std::cout << "   - " << node["title"].get<std::string>() << " | "
              << node["subtitle"].get<std::string>() << std::endl;
  }

  return 0;
}
