
function MyButton({onClick}) {
    // value === 0 means the square is empty.    
    return (
        <div
            className="booster"
            style={{backgroundColor: "#ec893b" }}
            onClick={onClick}
        >
            Colapsar iguales
        </div>
    );
}

export default MyButton;
