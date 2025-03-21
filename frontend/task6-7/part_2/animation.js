document.addEventListener('DOMContentLoaded', () => {
    const button_radio = document.querySelectorAll('.white_2nd_menu input[type="radio"][id^="second_menu_radio_"]');    const tableMap = new Map([
        ['second_menu_radio_1', '#Вычисления'],
        ['second_menu_radio_2', '#Бизнес_приложения'],
        ['second_menu_radio_3', '#Базы_данных'],
        ['second_menu_radio_4', '#ML_платформа'],
        ['second_menu_radio_1_1', '#Консалтинг_и_проектные_услуги'],
        ['second_menu_radio_2_1', '#Профессиональные_сервисы_и_аутсорсинг']
    ]);

    tableMap.forEach((tableId) => {
        const table = document.querySelector(`.white_2nd_menu_table${tableId}`);
        if (table) table.style.display = 'none';
    });

    const firstRadios = ['second_menu_radio_1', 'second_menu_radio_1_1'];
    firstRadios.forEach(radioId => {
        const firstRadio = document.getElementById(radioId);
        if (firstRadio && firstRadio.checked) {
            const firstTableId = tableMap.get(firstRadio.id);
            const firstTable = document.querySelector(`.white_2nd_menu_table${firstTableId}`);
            if (firstTable) firstTable.style.display = 'block';
        }
    });


    button_radio.forEach(button => {
        button.addEventListener('change', () => {
            tableMap.forEach((tableId, radioId) => {
                const table = document.querySelector(`.white_2nd_menu_table${tableId}`);
                if (button.id === radioId && button.checked) {
                    table.style.display = 'block';
                } else {
                    table.style.display = 'none';
                }
            });
        });
    });
});



const buttons = document.querySelectorAll('.center_menu .mybutton');
const buttons_main_menu = document.querySelectorAll('.main_menu_item');

buttons.forEach(button => {
    button.addEventListener('mouseover', () => {
        button.style.transform = 'scale(1.05)';
    });

    button.addEventListener('mouseleave', () => {
        button.style.transform = 'scale(1)';
    });
});

buttons_main_menu.forEach(curButton => {
    curButton.addEventListener('mouseover', () => {
        curButton.style.color = 'red';
    });

    curButton.addEventListener('mouseleave', () => {
        curButton.style.color = '';
    });
});

const grid_buttons = document.querySelectorAll('.grid_item');
grid_buttons.forEach(button => {
    button.addEventListener('mouseover', () => {
        button.style.backgroundColor = 'rgba(188, 195, 208, .6)';
    });

    button.addEventListener('mouseleave', () => {
        button.style.backgroundColor = '';
    });
})


const descriptionButtons = document.querySelectorAll('.white_2nd_menu_description_item');
const descriptionButtonStates = new Map();

descriptionButtons.forEach(button => {
    descriptionButtonStates.set(button, false);
});

const firstButtons = [descriptionButtons[0], descriptionButtons[6]];
if (firstButtons.length > 0) {
    firstButtons.forEach(button => {
        if (button) {
            button.style.backgroundColor = 'black';
            button.style.color = 'white';
            descriptionButtonStates.set(button, true);
        }
    });
}



const handlerDescriptionButtons = {
    get(target, prop) {
        const value = target[prop];

        if (prop === 'set' && typeof value === 'function') {
            return function (key, newValue) {
                value.call(target, key, newValue);
                const button = key;

                if (newValue) {
                    button.style.backgroundColor = 'black';
                    button.style.color = 'white';
                } else {
                    button.style.backgroundColor = 'white';
                    button.style.color = 'black';
                }

                return true;
            };
        }

        if (typeof value === 'function') {
            return value.bind(target);
        }

        return value;
    }
};

const proxiedDescriptionButtonStates = new Proxy(descriptionButtonStates, handlerDescriptionButtons);

descriptionButtons.forEach(button => {
    button.addEventListener('mouseover', () => {
        if (!proxiedDescriptionButtonStates.get(button)) {
            button.style.backgroundColor = 'rgba(188, 195, 208, .6)';
        }
    });

    const clickHandler = () => {
        descriptionButtons.forEach(curButton => {
            proxiedDescriptionButtonStates.set(curButton, false);
        });

        const isBlack = proxiedDescriptionButtonStates.get(button);
        proxiedDescriptionButtonStates.set(button, !isBlack);

        button.removeEventListener('click', clickHandler);
        setTimeout(() => {
            button.addEventListener('click', clickHandler);
        }, 200);
    };

    button.addEventListener('mouseleave', () => {
        if (!proxiedDescriptionButtonStates.get(button)) {
            button.style.backgroundColor = '';
        }
    });

    button.addEventListener('click', clickHandler);
});



const main_buttons = document.querySelectorAll('.main_menu_item');
const buttonStates = new Map();

const handler = {
    get(target, prop) {
        const value = target[prop];
        if (prop === 'set' && typeof value === 'function') {
            return function (key, newValue) {
                value.call(target, key, newValue);

                const buttonId = key.id;
                const subMenuId = `${buttonId}_white_2nd_menu`;
                console.log(buttonId)
                console.log(subMenuId)
                const subMenu = document.getElementById(subMenuId);
                // const subMenu = document.querySelector('.white_2nd_menu');
                if (subMenu) {
                    if (newValue) {
                        subMenu.classList.add('active');
                    } else {
                        subMenu.classList.remove('active');
                    }
                }

            }
        }
        if (typeof value === 'function') {
            return value.bind(target);
        }
        return value;
    }
};

const proxiedButtonStates = new Proxy(buttonStates, handler);

main_buttons.forEach(button => {
    proxiedButtonStates.set(button, false);
});

main_buttons.forEach(button => {
    function clickHandler() {
        const label = button.querySelector('.menu_label');
        const arrow = button.querySelector('.menu_arrow');
        const isRed = proxiedButtonStates.get(button);

        main_buttons.forEach(curButton => {
            const curLabel = curButton.querySelector('.menu_label');
            const curArrow = curButton.querySelector('.menu_arrow');

            if (curLabel) curLabel.style.color = '';
            if (curArrow) curArrow.style.color = '';
            if (curArrow) curArrow.classList.remove('rotated');
            proxiedButtonStates.set(curButton, false);
        });

        if (!isRed) {
            if (label) label.style.color = 'red';
            if (arrow) {
                arrow.style.color = 'red';
                arrow.classList.add('rotated');
            }
            proxiedButtonStates.set(button, true);
        }

        button.removeEventListener('click', clickHandler);
        setTimeout(() => {
            button.addEventListener('click', clickHandler);
        }, 200);
    }

    button.addEventListener('click', clickHandler);
});



