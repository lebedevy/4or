use four_4or::{interpreter::Interpreter, run};
use leptos::{ev::KeyboardEvent, logging::log, prelude::*};

const PREFIX: &str = ">  ";

fn main() {
    mount_to_body(App);
}

#[component]
fn App() -> impl IntoView {
    let ignore = vec!["Shift", "Alt", "CapsLock", "Escape", "Control"];

    let mut interpreter = Interpreter::new();
    let (input, set_input) = signal("".to_owned());
    let (history, set_history) = signal(Vec::<String>::new());

    let action = move |e: KeyboardEvent| {
        match (e.code().as_str(), e.key()) {
            ("Enter", _) => {
                let input = input.get();
                let mut history = set_history.write();
                (*history).push(input.clone());
                if let Err(err) = run(&mut interpreter, input.clone()) {
                    let err = err.get_error_text(&input);
                    history.append(&mut err.lines().map(|x| x.to_string()).collect());
                }
                *set_input.write() = "".to_owned();
                return;
            }
            ("Backspace", _) => {
                let mut val = set_input.write();
                let len = val.len();
                if len > 0 {
                    (*val).truncate(len - 1);
                }
            }
            (code, key) if ignore.iter().any(|x| key.starts_with(x)) => {
                log!("Ignored key '{}' with code '{}'", key, code)
            }
            (_, key) => *set_input.write() += key.as_str(),
        };
    };

    view! {
        <div >
            <div class:editor tabindex=0 autofocus on:keydown=action>
                {move || history.get().into_iter().map(|line| view! {
                    <Line>
                        {PREFIX}
                        {line}
                    </Line>
                }).collect::<Vec<_>>()}
                <Line>
                    {PREFIX}
                    {move || input.get()}
                    <Blinker />
                </Line>
            </div>
        </div>
    }
}

#[component]
fn Line(children: Children) -> impl IntoView {
    view! {
        <div class:flex style:flex-direction="row" style:align-items="center" style:white-space-collapse="preserve">{children()}</div>
    }
}

#[component]
fn Blinker() -> impl IntoView {
    view! {
        <div class:blinker class:blink />
    }
}
