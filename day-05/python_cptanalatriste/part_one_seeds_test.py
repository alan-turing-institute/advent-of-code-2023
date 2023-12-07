from python_cptanalatriste import part_one_seeds

seed_to_soil_lines: list[str] = ["50 98 2", "52 50 48"]
soil_to_fertiliser_lines: list[str] = ["0 15 37", "37 52 2", "39 0 15"]
fertiliser_to_water_lines: list[str] = [
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
]
water_to_light_lines: list[str] = ["88 18 7", "18 25 70"]
light_to_temperature_lines: list[str] = ["45 77 23", "81 45 19", "68 64 13"]
temperature_to_humidity_lines: list[str] = ["0 69 1", "1 0 69"]
humidity_to_location_lines: list[str] = ["60 56 37", "56 93 4"]


def test_parse_map() -> None:
    mapper: part_one_seeds.Mapper = part_one_seeds.Mapper(seed_to_soil_lines)

    seed_value: int = 0
    expected_soil: int = 0
    assert expected_soil == mapper.get_destination_from_source(seed_value)
    seed_value = 1
    expected_soil = 1
    assert expected_soil == mapper.get_destination_from_source(seed_value)

    seed_value = 48
    expected_soil = 48
    assert expected_soil == mapper.get_destination_from_source(seed_value)
    seed_value = 49
    expected_soil = 49
    assert expected_soil == mapper.get_destination_from_source(seed_value)
    seed_value = 50
    expected_soil = 52
    assert expected_soil == mapper.get_destination_from_source(seed_value)
    seed_value = 51
    expected_soil = 53
    assert expected_soil == mapper.get_destination_from_source(seed_value)

    seed_value = 96
    expected_soil = 98
    assert expected_soil == mapper.get_destination_from_source(seed_value)
    seed_value = 97
    expected_soil = 99
    assert expected_soil == mapper.get_destination_from_source(seed_value)


def test_parse_additional_maps() -> None:
    mapper: part_one_seeds.Mapper = part_one_seeds.Mapper(seed_to_soil_lines)

    seed_value = 98
    expected_soil = 50
    assert expected_soil == mapper.get_destination_from_source(seed_value)
    seed_value = 99
    expected_soil = 51
    assert expected_soil == mapper.get_destination_from_source(seed_value)


def test_parse_seeds() -> None:
    seed_to_soil: part_one_seeds.Mapper = part_one_seeds.Mapper(
        seed_to_soil_lines
    )
    soil_to_fertiliser: part_one_seeds.Mapper = part_one_seeds.Mapper(
        soil_to_fertiliser_lines
    )
    fertiliser_to_water: part_one_seeds.Mapper = part_one_seeds.Mapper(
        fertiliser_to_water_lines
    )
    water_to_light: part_one_seeds.Mapper = part_one_seeds.Mapper(
        water_to_light_lines
    )
    light_to_temperature: part_one_seeds.Mapper = part_one_seeds.Mapper(
        light_to_temperature_lines
    )
    temperature_to_humidity: part_one_seeds.Mapper = part_one_seeds.Mapper(
        temperature_to_humidity_lines
    )
    humidity_to_location: part_one_seeds.Mapper = part_one_seeds.Mapper(
        humidity_to_location_lines
    )

    analyser: part_one_seeds.SeedAnaliser = part_one_seeds.SeedAnaliser(
        seed_to_soil=seed_to_soil,
        soil_to_fertiliser=soil_to_fertiliser,
        fertiliser_to_water=fertiliser_to_water,
        water_to_light=water_to_light,
        light_to_temperature=light_to_temperature,
        temperature_to_humidity=temperature_to_humidity,
        humidity_to_location=humidity_to_location,
    )

    assert analyser.analyse_seed(79) == {
        "soil": 81,
        "fertiliser": 81,
        "water": 81,
        "light": 74,
        "temperature": 78,
        "humidity": 78,
        "location": 82,
    }

    assert analyser.analyse_seed(14) == {
        "soil": 14,
        "fertiliser": 53,
        "water": 49,
        "light": 42,
        "temperature": 42,
        "humidity": 43,
        "location": 43,
    }

    assert analyser.analyse_seed(55) == {
        "soil": 57,
        "fertiliser": 57,
        "water": 53,
        "light": 46,
        "temperature": 82,
        "humidity": 82,
        "location": 86,
    }


def test_process_file() -> None:
    assert 35 == part_one_seeds.main(
        "day-05/python_cptanalatriste/sample_input.txt"
    )
