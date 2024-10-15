use pcsc::{Context, Protocols, ReaderState, Scope, ShareMode, State, MAX_BUFFER_SIZE};
use rocket::{fairing::AdHoc, http::Header};

#[macro_use] extern crate rocket;

#[derive(Responder)]
#[response(status = 500)]
struct NfcReaderError{
    message: String
}

#[get("/nfc-card-id")]
fn get_nfc_card_id() -> Result<String, NfcReaderError> {
    let ctx = Context::establish(Scope::System).map_err(|e| NfcReaderError { message: format!("Context::establish failed: {e}") })?;
    let names = ctx.list_readers_owned().map_err(|e| NfcReaderError { message: format!("Context::list_readers failed: {e}") })?;
    println!("Found {} readers:", names.len());
    for name in &names {
        println!("* {:?}", name);
    }
    let reader_name = names.first().ok_or(NfcReaderError { message: format!("No reader found") })?;
    let mut reader_states = vec![ReaderState::new(reader_name.clone(), State::UNAWARE)];
    loop {
        ctx.get_status_change(None, &mut reader_states).map_err(|e| NfcReaderError { message: format!("Context::get_status_change failed: {e}") })?;
        println!("Reader state: {:?}", reader_states[0].event_state());
        if reader_states[0].event_state().contains(State::PRESENT) {
            break
        }
        reader_states[0].sync_current_state();
    }
    let card = ctx.connect(&reader_name, ShareMode::Shared, Protocols::ANY).map_err(|e| NfcReaderError { message: format!("Context::connect failed: {e}") })?;
    let get_serial_number_command = [0xFFu8, 0xCA, 0x00, 0x00, 0x00];
    let mut serial_number_buffer = [0; MAX_BUFFER_SIZE];
    let serial_number = card.transmit(&get_serial_number_command, &mut serial_number_buffer).map_err(|e| NfcReaderError { message: format!("Card::transmit failed: {e}") })?;
    let serial_number_as_string = serial_number.into_iter().map(|v| format!("{:02X?}", v)).collect::<Vec<_>>().join("");
    Ok(serial_number_as_string)
}

#[launch]
fn rocket() -> _ {
    rocket::build()
        .attach(AdHoc::on_response("Enable CORS", |_req, res| Box::pin(async move {
            res.set_header(Header::new("Access-Control-Allow-Origin", "*"));
        })))
        .mount("/", routes![get_nfc_card_id])
}
